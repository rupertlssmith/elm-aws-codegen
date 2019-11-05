module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel, Endpoint)
import AWSService exposing (AWSService, AWSType(..), Operation, Shape, ShapeRef)
import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Restricted(..), Type(..))
import Maybe.Extra


transform : AWSService -> AWSApiModel
transform service =
    let
        default =
            AWSApiModel.example

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

        _ =
            Debug.log "errors" errMappings

        _ =
            Debug.log "errors" errOperations
    in
    { default
        | declarations = okMappings
        , operations = okOperations
    }



--== Error reporting.


type Error
    = SingleError String
    | MultipleError (List Error)


error : String -> Error
error val =
    SingleError val


errors : List Error -> Error
errors errs =
    MultipleError errs


addError : String -> Error -> Error
addError val errs =
    case errs of
        SingleError single ->
            MultipleError [ SingleError val, SingleError single ]

        MultipleError errorList ->
            MultipleError (SingleError val :: errorList)



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


modelShapes : Dict String Shape -> Dict String Outline -> Dict String (Result Error Declarable)
modelShapes shapeDict outlineDict =
    Dict.map
        (\key value -> modelShape outlineDict value key)
        shapeDict


modelShape : Dict String Outline -> Shape -> String -> Result Error Declarable
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
            error "Blob not implemented." |> Err

        AStructure ->
            modelStructure outlineDict shape name

        AList ->
            modelList outlineDict shape name

        AMap ->
            modelMap outlineDict shape name

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            error "Unknown not implemented." |> Err


modelString : Dict String Outline -> Shape -> String -> Result Error Declarable
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


modelInt : Dict String Outline -> Shape -> String -> Result Error Declarable
modelInt outlineDict shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            RInt { min = shape.min, max = shape.max, width = Nothing }
                |> DRestricted
                |> Ok

        _ ->
            BInt |> TBasic |> DAlias |> Ok


modelStructure : Dict String Outline -> Shape -> String -> Result Error Declarable
modelStructure outlineDict shape name =
    case shape.members of
        Nothing ->
            error (name ++ ": structure has no members") |> Err

        Just members ->
            let
                ( fieldErrors, fields ) =
                    Dict.foldl
                        (\memberName shapeRef ( errAccum, fieldAccum ) ->
                            case shapeRefToL1Type shapeRef outlineDict of
                                Nothing ->
                                    ( error "Structure .members reference did no resolve." :: errAccum
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
                    errors fieldErrors |> Err


modelList : Dict String Outline -> Shape -> String -> Result Error Declarable
modelList outlineDict shape name =
    case shape.member of
        Nothing ->
            error "List .member is empty, but should be a shape reference." |> Err

        Just ref ->
            case shapeRefToL1Type ref outlineDict of
                Just type_ ->
                    CList type_ |> TContainer |> DAlias |> Ok

                Nothing ->
                    error "List .member reference did not resolve." |> Err


modelMap : Dict String Outline -> Shape -> String -> Result Error Declarable
modelMap outlineDict shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    error "Map .key is empty, should be basic or enum." |> Err

                Just keyRef ->
                    case shapeRefToL1Type keyRef outlineDict of
                        Just type_ ->
                            if shapeRefIsEnum keyRef outlineDict then
                                type_ |> Ok

                            else if shapeRefIsBasic keyRef outlineDict then
                                type_ |> Ok

                            else
                                error "Map .key is not an enum or basic." |> Err

                        Nothing ->
                            error "Map .key reference did not resolve." |> Err

        valTypeRes =
            case shape.value of
                Nothing ->
                    error "Map .value is empty, should be basic or enum." |> Err

                Just valRef ->
                    case shapeRefToL1Type valRef outlineDict of
                        Just type_ ->
                            type_ |> Ok

                        Nothing ->
                            error "Map .value reference did not resolve." |> Err
    in
    case ( keyTypeRes, valTypeRes ) of
        ( Err keyError, Err valError ) ->
            errors [ keyError, valError ] |> Err

        ( Err keyError, _ ) ->
            Err keyError

        ( _, Err valError ) ->
            Err valError

        ( Ok keyType, Ok valType ) ->
            CDict keyType valType |> TContainer |> DAlias |> Ok



--== Operations


modelOperations : Dict String Declarable -> Dict String Operation -> Dict String (Result Error Endpoint)
modelOperations okMappings operations =
    Dict.empty
        |> Dict.insert "test" (Ok { request = TBasic BInt, response = TBasic BInt })
