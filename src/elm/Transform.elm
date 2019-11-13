module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel, Endpoint)
import AWSService exposing (AWSService, AWSType(..), Operation, Shape, ShapeRef)
import Console
import Dict exposing (Dict)
import Enum exposing (Enum)
import Errors exposing (Error)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Restricted(..), Type(..))
import Maybe.Extra


type TransformError
    = NoMembers String
    | UnresolvedMemberRef
    | NoRequestType
    | UnresolvedMapKeyRef
    | MapKeyNotEnumOrBasic
    | MapKeyEmpty
    | MapValueEmpty
    | UnresolvedMapValueRef
    | UnresolvedListMemberRef
    | ListMemberEmpty
    | BlobNotImplemented
    | UnknownNotImplemented


errorToString : TransformError -> String
errorToString err =
    case err of
        NoMembers name ->
            name ++ ": structure has no members"

        UnresolvedMemberRef ->
            "Structure .members reference did no resolve."

        NoRequestType ->
            "No request type."

        UnresolvedMapKeyRef ->
            "Map .key reference did not resolve."

        MapKeyNotEnumOrBasic ->
            "Map .key is not an enum or basic."

        MapKeyEmpty ->
            "Map .key is empty, should be basic or enum."

        MapValueEmpty ->
            "Map .value is empty, should be basic or enum."

        UnresolvedMapValueRef ->
            "Map .value reference did not resolve."

        UnresolvedListMemberRef ->
            "List .member reference did not resolve."

        ListMemberEmpty ->
            "List .member is empty, but should be a shape reference."

        BlobNotImplemented ->
            "Blob not implemented."

        UnknownNotImplemented ->
            "Unknown not implemented."


transform : AWSService -> ( AWSApiModel, List String )
transform service =
    let
        outlineDict =
            outline service.shapes

        mappings =
            modelShapes service.shapes outlineDict

        ( okMappings, errMappings ) =
            Dict.foldl
                (\key val ( okAccum, errAccum ) ->
                    case val of
                        Ok decl ->
                            ( Dict.insert key decl okAccum, errAccum )

                        Err err ->
                            ( okAccum, Dict.insert key err errAccum )
                )
                ( Dict.empty, Dict.empty )
                mappings

        operations =
            modelOperations okMappings service.operations

        ( okOperations, errOperations ) =
            Dict.foldl
                (\key val ( okAccum, errAccum ) ->
                    case val of
                        Ok endpoint ->
                            ( Dict.insert key endpoint okAccum, errAccum )

                        Err err ->
                            ( okAccum, Dict.insert key err errAccum )
                )
                ( Dict.empty, Dict.empty )
                operations

        enrichError key error =
            Errors.map (\err -> Console.fgCyan ++ key ++ Console.reset ++ ": " ++ errorToString err) error

        transformErrors =
            [ Dict.map enrichError errMappings |> Dict.values
            , Dict.map enrichError errOperations |> Dict.values
            ]
                |> List.concat
                |> Errors.combine
                |> Errors.toList
    in
    ( { declarations = okMappings
      , operations = okOperations
      , name = [ "AWS", service.metaData.serviceId ]
      , isRegional = Maybe.Extra.isJust service.metaData.globalEndpoint
      , endpointPrefix = service.metaData.endpointPrefix
      , apiVersion = service.metaData.apiVersion
      , protocol = service.metaData.protocol
      , signer =
            case service.metaData.signatureVersion of
                Just "v4" ->
                    "signV4"

                Just "s3" ->
                    "signS3"

                _ ->
                    "signV4"
      , docs = Maybe.withDefault "" service.documentation
      , xmlNamespace = service.metaData.xmlNamespace
      , targetPrefix = service.metaData.targetPrefix
      , signingName = service.metaData.signingName
      , jsonVersion = service.metaData.jsonVersion
      }
    , transformErrors
    )



--== Error reporting.
--== First pass.
-- In the first pass all the named shapes are discovered and an approximate
-- outline of how they will transalate into L1 is generated. Either they are
-- basic types, or refer to things that will be given declared names.


type Outline
    = OlNamed String
    | OlEnum String
    | OlBasic Basic


outline : Dict String Shape -> Dict String Outline
outline shapes =
    Dict.foldl
        (\key value accum ->
            case outlineShape value key of
                Nothing ->
                    accum

                Just ol ->
                    Dict.insert key ol accum
        )
        Dict.empty
        shapes


outlineShape : Shape -> String -> Maybe Outline
outlineShape shape name =
    case shape.type_ of
        AString ->
            outlineString shape name |> Just

        ABoolean ->
            BBool |> OlBasic |> Just

        AInteger ->
            outlineInt shape name |> Just

        ALong ->
            BInt |> OlBasic |> Just

        AFloat ->
            BReal |> OlBasic |> Just

        ADouble ->
            BReal |> OlBasic |> Just

        ABlob ->
            Nothing

        AStructure ->
            name |> OlNamed |> Just

        AList ->
            name |> OlNamed |> Just

        AMap ->
            name |> OlNamed |> Just

        ATimestamp ->
            name |> OlNamed |> Just

        AUnknown ->
            Nothing


outlineString : Shape -> String -> Outline
outlineString shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            OlEnum name

        ( Nothing, True ) ->
            OlNamed name

        ( _, _ ) ->
            OlBasic BString


outlineInt : Shape -> String -> Outline
outlineInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            OlNamed name

        _ ->
            OlBasic BInt


shapeRefToL1Type : ShapeRef -> Dict String Outline -> Maybe Type
shapeRefToL1Type ref outlineDict =
    case Dict.get ref.shape outlineDict of
        Just (OlNamed memberName) ->
            TNamed memberName |> Just

        Just (OlEnum memberName) ->
            TNamed memberName |> Just

        Just (OlBasic basic) ->
            TBasic basic |> Just

        Nothing ->
            Nothing


shapeRefIsEnum : ShapeRef -> Dict String Outline -> Bool
shapeRefIsEnum ref outlineDict =
    case Dict.get ref.shape outlineDict of
        Just (OlEnum memberName) ->
            True

        _ ->
            False


shapeRefIsBasic : ShapeRef -> Dict String Outline -> Bool
shapeRefIsBasic ref outlineDict =
    case Dict.get ref.shape outlineDict of
        Just (OlBasic basic) ->
            True

        _ ->
            False



--== Second pass.
-- In the second pass a complete L1 model is generated for each shape. The
-- outline from the first pass is used to guide this, as all basic types are
-- inlined without being given intermediate names, and all types with declared
-- names will be used by referring to them.


modelShapes : Dict String Shape -> Dict String Outline -> Dict String (Result (Error TransformError) Declarable)
modelShapes shapeDict outlineDict =
    Dict.map
        (\key value -> modelShape outlineDict value key)
        shapeDict


modelShape : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelShape outlineDict shape name =
    case shape.type_ of
        AString ->
            modelString outlineDict shape name

        ABoolean ->
            BBool |> TBasic |> DAlias |> Ok

        AInteger ->
            modelInt outlineDict shape name

        ALong ->
            BInt |> TBasic |> DAlias |> Ok

        AFloat ->
            BReal |> TBasic |> DAlias |> Ok

        ADouble ->
            BReal |> TBasic |> DAlias |> Ok

        ABlob ->
            Errors.single BlobNotImplemented |> Err

        AStructure ->
            modelStructure outlineDict shape name

        AList ->
            modelList outlineDict shape name

        AMap ->
            modelMap outlineDict shape name

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            Errors.single UnknownNotImplemented |> Err


modelString : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelString outlineDict shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            enumVals
                |> DEnum
                |> Ok

        ( Nothing, True ) ->
            RString { minLength = shape.min, maxLength = shape.max, regex = shape.pattern }
                |> DRestricted
                |> Ok

        ( _, _ ) ->
            BString |> TBasic |> DAlias |> Ok


modelInt : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelInt outlineDict shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            RInt { min = shape.min, max = shape.max, width = Nothing }
                |> DRestricted
                |> Ok

        _ ->
            BInt |> TBasic |> DAlias |> Ok


modelStructure : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelStructure outlineDict shape name =
    case shape.members of
        Nothing ->
            NoMembers name |> Errors.single |> Err

        Just members ->
            let
                ( fieldErrors, fields ) =
                    Dict.foldl
                        (\memberName shapeRef ( errAccum, fieldAccum ) ->
                            case shapeRefToL1Type shapeRef outlineDict of
                                Nothing ->
                                    ( Errors.single UnresolvedMemberRef :: errAccum
                                    , fieldAccum
                                    )

                                Just type_ ->
                                    ( errAccum
                                    , ( memberName, type_ ) :: fieldAccum
                                    )
                        )
                        ( [], [] )
                        members
            in
            case fieldErrors of
                [] ->
                    fields |> TProduct |> DAlias |> Ok

                _ ->
                    Errors.combine fieldErrors |> Err


modelList : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelList outlineDict shape name =
    case shape.member of
        Nothing ->
            Errors.single ListMemberEmpty |> Err

        Just ref ->
            case shapeRefToL1Type ref outlineDict of
                Just type_ ->
                    CList type_ |> TContainer |> DAlias |> Ok

                Nothing ->
                    Errors.single UnresolvedListMemberRef |> Err


modelMap : Dict String Outline -> Shape -> String -> Result (Error TransformError) Declarable
modelMap outlineDict shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    Errors.single MapKeyEmpty |> Err

                Just keyRef ->
                    case shapeRefToL1Type keyRef outlineDict of
                        Just type_ ->
                            if shapeRefIsEnum keyRef outlineDict then
                                type_ |> Ok

                            else if shapeRefIsBasic keyRef outlineDict then
                                type_ |> Ok

                            else
                                Errors.single MapKeyNotEnumOrBasic |> Err

                        Nothing ->
                            Errors.single UnresolvedMapKeyRef |> Err

        valTypeRes =
            case shape.value of
                Nothing ->
                    Errors.single MapValueEmpty |> Err

                Just valRef ->
                    case shapeRefToL1Type valRef outlineDict of
                        Just type_ ->
                            type_ |> Ok

                        Nothing ->
                            Errors.single UnresolvedMapValueRef |> Err
    in
    case ( keyTypeRes, valTypeRes ) of
        ( Err keyError, Err valError ) ->
            Errors.combine [ keyError, valError ] |> Err

        ( Err keyError, _ ) ->
            Err keyError

        ( _, Err valError ) ->
            Err valError

        ( Ok keyType, Ok valType ) ->
            CDict keyType valType |> TContainer |> DAlias |> Ok



--== Operations


modelOperations : Dict String Declarable -> Dict String Operation -> Dict String (Result (Error TransformError) Endpoint)
modelOperations typeDict operations =
    Dict.map
        (\name operation -> modelOperation typeDict name operation)
        operations


modelOperation : Dict String Declarable -> String -> Operation -> Result (Error TransformError) Endpoint
modelOperation typeDict name operation =
    let
        requestTypeRes =
            operation.input
                |> Maybe.map .shape
                |> Maybe.andThen
                    (\reqTypeName ->
                        Dict.get reqTypeName typeDict
                            |> Maybe.map (always reqTypeName)
                    )

        responseTypeRes =
            operation.output
                |> Maybe.map .shape
                |> Maybe.andThen
                    (\respTypeName ->
                        Dict.get respTypeName typeDict
                            |> Maybe.map (always respTypeName)
                    )
    in
    case requestTypeRes of
        Nothing ->
            Errors.single NoRequestType |> Err

        Just requestTypeName ->
            case responseTypeRes of
                Nothing ->
                    Ok
                        { httpMethod = operation.http.method
                        , url = "/"
                        , request = ( requestTypeName, TNamed requestTypeName )
                        , response = Nothing
                        }

                Just responseTypeName ->
                    Ok
                        { httpMethod = operation.http.method
                        , url = "/"
                        , request = ( requestTypeName, TNamed requestTypeName )
                        , response = Just ( responseTypeName, TNamed responseTypeName )
                        }
