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
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, L1, Restricted(..), Type(..), Unchecked)
import L2 exposing (RefChecked(..))
import List.Nonempty
import Maybe.Extra
import MultiError exposing (ResultME)
import String.Case as Case


type TransformError
    = UnresolvedRef String
    | NoMembers String
    | MapKeyTypeNotAllowed
    | MapKeyEmpty
    | MapValueEmpty
    | ListMemberEmpty
    | UnknownNotImplemented


type Outlined
    = OlBasic Basic
    | OlEnum String
    | OlRestricted String Basic
    | OlNamed String


enrichError key error =
    List.Nonempty.map (\err -> Console.fgCyan ++ key ++ Console.reset ++ ": " ++ errorToString err) error


errorToString : TransformError -> String
errorToString err =
    case err of
        UnresolvedRef hint ->
            hint ++ " reference did not resolve."

        NoMembers name ->
            name ++ ": structure has no members"

        MapKeyTypeNotAllowed ->
            "Map .key is not an enum, restricted, or basic."

        MapKeyEmpty ->
            "Map .key is empty."

        MapValueEmpty ->
            "Map .value is empty."

        ListMemberEmpty ->
            "List .member is empty, but should be a shape reference."

        UnknownNotImplemented ->
            "Unknown not implemented."


transform : AWSService -> ResultME TransformError AWSApiModel
transform service =
    let
        outlineDict =
            outline service.shapes

        -- outlineDict =
        --     Dict.empty
        mappingsResult : ResultME TransformError (Dict String (Declarable RefChecked))
        mappingsResult =
            modelShapes service.shapes

        -- l2mappingsResult =
        --     mappingsResult
        --         |> MultiError.andThen Checker.check
        operationsResult : ResultME TransformError (Dict String Endpoint)
        operationsResult =
            mappingsResult
                |> MultiError.andThen (modelOperations service.operations)

        mappingsAndOperations =
            MultiError.combine2
                Tuple.pair
                mappingsResult
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



--== First pass.
-- In the first pass all the named shapes are discovered and an approximate
-- outline of how they will translate into L1 is generated. Either they are
-- basic types, or refer to things that will be given declared names.


outline : Dict String Shape -> Dict String Outlined
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


outlineShape : Shape -> String -> Maybe Outlined
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
            BString |> OlBasic |> Just

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


outlineString : Shape -> String -> Outlined
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
            OlRestricted name BString

        ( _, _ ) ->
            OlBasic BString


outlineInt : Shape -> String -> Outlined
outlineInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            OlRestricted name BInt

        _ ->
            OlBasic BInt


shapeRefToL1Type : ShapeRef -> Dict String Outlined -> Maybe (Type RefChecked)
shapeRefToL1Type ref outlineDict =
    case Dict.get ref.shape outlineDict of
        Just (OlNamed memberName) ->
            TNamed memberName RcNone |> Just

        Just (OlEnum memberName) ->
            TNamed memberName RcEnum |> Just

        Just (OlRestricted memberName basic) ->
            TNamed memberName (RcRestricted basic) |> Just

        Just (OlBasic basic) ->
            TBasic basic |> Just

        Nothing ->
            Nothing



--== Second pass.
-- In the second pass a complete L1 model is generated for each shape. The
-- outline from the first pass is used to guide this, as all basic types are
-- inlined without being given intermediate names, and all types with declared
-- names will be used by referring to them.


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
            case shapeRefToL1Type shapeRef outlineDict of
                Nothing ->
                    ( MultiError.error (UnresolvedRef "Structure .members") :: errAccum
                    , fieldAccum
                    )

                Just type_ ->
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
            let
                ( fieldErrors, fields ) =
                    Dict.foldl modelField ( [], [] ) members
            in
            case fieldErrors of
                [] ->
                    case List.Nonempty.fromList fields of
                        Just nonemptyFields ->
                            nonemptyFields |> TProduct |> DAlias |> Ok

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
            case shapeRefToL1Type ref outlineDict of
                Just type_ ->
                    CList type_ |> TContainer |> DAlias |> Ok

                Nothing ->
                    UnresolvedRef "List .member" |> MultiError.error


modelMap : Shape -> String -> ResultME TransformError (Declarable Unchecked)
modelMap shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    MapKeyEmpty |> MultiError.error

                Just keyRef ->
                    case shapeRefToL1Type keyRef outlineDict of
                        Just type_ ->
                            if shapeRefIsEnum keyRef outlineDict then
                                type_ |> Ok

                            else if shapeRefIsRestricted keyRef outlineDict then
                                type_ |> Ok

                            else if shapeRefIsBasic keyRef outlineDict then
                                type_ |> Ok

                            else
                                MapKeyTypeNotAllowed |> MultiError.error

                        Nothing ->
                            UnresolvedRef "Map .key" |> MultiError.error

        valTypeRes =
            case shape.value of
                Nothing ->
                    MapValueEmpty |> MultiError.error

                Just valRef ->
                    case shapeRefToL1Type valRef outlineDict of
                        Just type_ ->
                            type_ |> Ok

                        Nothing ->
                            UnresolvedRef "Map .value" |> MultiError.error
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
    let
        paramType opShapeRef errHint =
            case opShapeRef of
                Nothing ->
                    TUnit |> Ok

                Just shapeRef ->
                    case Dict.get shapeRef.shape typeDict of
                        Just decl ->
                            TNamed shapeRef.shape RcNone |> Ok

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
