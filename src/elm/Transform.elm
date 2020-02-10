module Transform exposing (transform)

import AWS.Core.Service exposing (Signer(..))
import AWSApiModel exposing (AWSApiModel, Endpoint)
import AWSService exposing (AWSService, AWSType(..), Operation, Shape, ShapeRef)
import Checker
import Console
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Comment, DocComment, FileComment)
import Enum exposing (Enum)
import Html.Parser as HP
import L1 exposing (Basic(..), Container(..), Declarable(..), L1, Restricted(..), Type(..), Unchecked(..))
import L2 exposing (L2, RefChecked(..))
import List.Nonempty
import Maybe.Extra
import MultiError exposing (ResultME)
import Naming
import String.Case as Case


type TransformError
    = UnresolvedRef String
    | NoMembers String
    | MapKeyEmpty
    | MapValueEmpty
    | ListMemberEmpty
    | UnknownNotImplemented


errorToString : TransformError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        NoMembers name ->
            name ++ ": structure has no members"

        MapKeyEmpty ->
            "Map .key is empty."

        MapValueEmpty ->
            "Map .value is empty."

        ListMemberEmpty ->
            "List .member is empty, but should be a shape reference."

        UnknownNotImplemented ->
            "Unknown not implemented."


transform : AWSService -> ResultME String AWSApiModel
transform service =
    let
        mappingsResult : ResultME String (Dict String (Declarable Unchecked))
        mappingsResult =
            modelShapes service.shapes
                |> MultiError.mapError errorToString

        l2mappingsResult =
            mappingsResult
                |> MultiError.andThen
                    (Checker.check >> MultiError.mapError Checker.errorToString)

        operationsResult : ResultME String (Dict String Endpoint)
        operationsResult =
            l2mappingsResult
                |> MultiError.andThen
                    (modelOperations service.operations
                        >> MultiError.mapError errorToString
                    )

        mappingsAndOperations =
            MultiError.combine2
                Tuple.pair
                l2mappingsResult
                operationsResult
    in
    MultiError.map
        (\( mappings, operations ) ->
            { declarations = mappings
            , operations = operations
            , name = [ "AWS", Case.toCamelCaseUpper service.metaData.serviceId ]
            , isRegional = Maybe.Extra.isNothing service.metaData.globalEndpoint
            , endpointPrefix = service.metaData.endpointPrefix
            , apiVersion = service.metaData.apiVersion
            , protocol = service.metaData.protocol
            , signer =
                case service.metaData.signatureVersion of
                    Just signer ->
                        signer

                    _ ->
                        SignV4
            , xmlNamespace = service.metaData.xmlNamespace
            , targetPrefix = service.metaData.targetPrefix
            , signingName = service.metaData.signingName
            , jsonVersion = service.metaData.jsonVersion
            , documentation = Maybe.map htmlToFileComment service.documentation
            }
        )
        mappingsAndOperations


shapeRefToL1Type : ShapeRef -> Type Unchecked
shapeRefToL1Type ref =
    TNamed ref.shape Unchecked



--=== L1 Model Assembly Pass
-- A complete L1 model is generated for each shape.
-- Errors in the shape definitions are detected, but checking of the L1 model
-- is handled when it is lowered into L2.


modelShapes :
    Dict String Shape
    -> ResultME TransformError L1
modelShapes shapeDict =
    Dict.map
        (\key value -> modelShape value key)
        shapeDict
        |> MultiError.combineDict


modelShape : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelShape shape name =
    case shape.type_ of
        AString ->
            modelString shape name

        ABoolean ->
            BBool |> TBasic |> DAlias |> Ok

        AInteger ->
            modelInt shape name

        ALong ->
            BInt |> TBasic |> DAlias |> Ok

        AFloat ->
            BReal |> TBasic |> DAlias |> Ok

        ADouble ->
            BReal |> TBasic |> DAlias |> Ok

        ABlob ->
            BString |> TBasic |> DAlias |> Ok

        AStructure ->
            modelStructure shape name

        AList ->
            modelList shape name

        AMap ->
            modelMap shape name

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            MultiError.error UnknownNotImplemented


modelString : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelString shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            case List.Nonempty.fromList enumVals of
                Just nonemptyEnumVals ->
                    nonemptyEnumVals
                        |> DEnum
                        |> Ok

                Nothing ->
                    NoMembers name |> MultiError.error

        ( Nothing, True ) ->
            RString { minLength = shape.min, maxLength = shape.max, regex = shape.pattern }
                |> DRestricted
                |> Ok

        ( _, _ ) ->
            BString |> TBasic |> DAlias |> Ok


modelInt : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            RInt { min = shape.min, max = shape.max, width = Nothing }
                |> DRestricted
                |> Ok

        _ ->
            BInt |> TBasic |> DAlias |> Ok


modelStructure : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelStructure shape name =
    let
        -- shape.required lists names of fields that are required.
        modelField memberName shapeRef ( errAccum, fieldAccum ) =
            let
                type_ =
                    shapeRefToL1Type shapeRef
            in
            case shape.required of
                Nothing ->
                    ( errAccum
                    , ( memberName, type_ |> COptional |> TContainer ) :: fieldAccum
                    )

                Just requiredFields ->
                    if List.member memberName requiredFields then
                        ( errAccum
                        , ( memberName, type_ ) :: fieldAccum
                        )

                    else
                        ( errAccum
                        , ( memberName, type_ |> COptional |> TContainer ) :: fieldAccum
                        )
    in
    case shape.members of
        Nothing ->
            NoMembers name |> MultiError.error

        Just members ->
            -- TODO: Rewrite this part as should just combine errors over the
            -- fields.
            let
                ( fieldErrors, fields ) =
                    Dict.foldl modelField ( [], [] ) members
            in
            case fieldErrors of
                [] ->
                    case List.Nonempty.fromList fields of
                        Just nonemptyFields ->
                            nonemptyFields
                                |> Naming.sortNonemptyNamed
                                |> TProduct
                                |> DAlias
                                |> Ok

                        Nothing ->
                            TEmptyProduct |> DAlias |> Ok

                err :: errs ->
                    -- MultiError.errors err errs
                    Debug.todo "Fix error handling"


modelList : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelList shape name =
    case shape.member of
        Nothing ->
            ListMemberEmpty |> MultiError.error

        Just ref ->
            shapeRefToL1Type ref |> CList |> TContainer |> DAlias |> Ok


modelMap : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelMap shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    MapKeyEmpty |> MultiError.error

                Just keyRef ->
                    shapeRefToL1Type keyRef |> Ok

        valTypeRes =
            case shape.value of
                Nothing ->
                    MapValueEmpty |> MultiError.error

                Just valRef ->
                    shapeRefToL1Type valRef |> Ok
    in
    MultiError.combine2
        (\keyType valType -> CDict keyType valType |> TContainer |> DAlias)
        keyTypeRes
        valTypeRes



--== Operations


modelOperations :
    Dict String Operation
    -> Dict String (Declarable RefChecked)
    -> ResultME TransformError (Dict String Endpoint)
modelOperations operations typeDict =
    Dict.map
        (\name operation -> modelOperation typeDict name operation)
        operations
        |> MultiError.combineDict


modelOperation : Dict String (Declarable RefChecked) -> String -> Operation -> ResultME TransformError Endpoint
modelOperation typeDict name operation =
    -- TODO: The ref checking should be done by the L2 checker.
    let
        paramType opShapeRef errHint =
            case opShapeRef of
                Nothing ->
                    TUnit |> Ok

                Just shapeRef ->
                    case Dict.get shapeRef.shape typeDict of
                        Just decl ->
                            TNamed shapeRef.shape RcTUnit |> Ok

                        Nothing ->
                            UnresolvedRef "Input" |> MultiError.error

        requestRes =
            paramType operation.input "Input"

        responseRes =
            paramType operation.output "Output"
    in
    MultiError.combine2
        (\request response ->
            { httpMethod = operation.http.method
            , url = operation.http.requestUri |> Maybe.withDefault "/"
            , request = request
            , response = response
            , documentation = Maybe.map htmlToDocComment operation.documentation
            }
        )
        requestRes
        responseRes



--== HTML Documentation to Markdown conversion.


htmlToFileComment : String -> Comment FileComment
htmlToFileComment val =
    let
        parsedHtmlResult =
            HP.run val

        empty =
            CG.emptyFileComment
    in
    case parsedHtmlResult of
        Err _ ->
            empty
                |> CG.markdown val

        Ok nodes ->
            htmlToComment nodes True [] empty
                |> Tuple.second


htmlToDocComment : String -> Comment DocComment
htmlToDocComment val =
    let
        parsedHtmlResult =
            HP.run val

        empty =
            CG.emptyDocComment
    in
    case parsedHtmlResult of
        Err _ ->
            empty
                |> CG.markdown val

        Ok nodes ->
            htmlToComment nodes True [] empty
                |> Tuple.second


htmlToComment : List HP.Node -> Bool -> List String -> Comment a -> ( List String, Comment a )
htmlToComment nodes isTop accum comment =
    case nodes of
        [] ->
            ( accum, comment )

        node :: ns ->
            let
                ( innerAccum, innerComment ) =
                    nodeToComment node isTop accum comment
            in
            htmlToComment ns isTop innerAccum innerComment


nodeToComment : HP.Node -> Bool -> List String -> Comment a -> ( List String, Comment a )
nodeToComment node isTop accum comment =
    case node of
        HP.Text text ->
            ( text :: accum, comment )

        HP.Element el attr children ->
            let
                ( innerAccum, innerComment ) =
                    htmlToComment children False accum comment

                ( taggedAccum, taggedInnerComment ) =
                    case ( el, innerAccum ) of
                        -- ( "p", _ ) ->
                        --     ( [], CG.markdown (List.reverse innerAccum |> String.join "") innerComment )
                        ( "fullname", hd :: tl ) ->
                            ( ("## " ++ hd) :: tl, innerComment )

                        -- ( "ul", _ ) ->
                        --     ( [], CG.markdown (List.reverse innerAccum |> String.join "") innerComment )
                        ( "li", _ ) ->
                            let
                                _ =
                                    Debug.log "innerAccum" innerAccum
                            in
                            ( [ "\n - " ++ (List.reverse innerAccum |> String.join "") ]
                            , innerComment
                            )

                        ( "code", hd :: tl ) ->
                            ( ("`" ++ hd ++ "`") :: tl, innerComment )

                        ( "a", hd :: tl ) ->
                            ( ("`" ++ hd ++ "`") :: tl, innerComment )

                        _ ->
                            ( innerAccum, innerComment )
            in
            if isTop then
                ( [], CG.markdown (List.reverse taggedAccum |> String.join "") taggedInnerComment )

            else
                ( taggedAccum, taggedInnerComment )

        HP.Comment _ ->
            ( accum, comment )
