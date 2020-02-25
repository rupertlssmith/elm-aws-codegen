module Transform exposing (transform)

import AWS.Core.Service exposing (Protocol(..), Signer(..))
import AWSService exposing (AWSService, AWSType(..), Operation, Shape, ShapeRef)
import Checker
import Console
import Dict exposing (Dict)
import Enum exposing (Enum)
import HttpMethod exposing (HttpMethod(..))
import L1
    exposing
        ( Basic(..)
        , Container(..)
        , Declarable(..)
        , L1
        , PropSpec(..)
        , Property(..)
        , Restricted(..)
        , Type(..)
        , Unchecked(..)
        )
import L2 exposing (L2, RefChecked(..))
import L3 exposing (L3)
import List.Nonempty
import Maybe.Extra
import Naming
import ResultME exposing (ResultME)
import String.Case as Case
import Templates.AWSStubs as AWSStubs


type TransformError pos
    = UnresolvedRef pos String
    | NoMembers pos String
    | MapKeyEmpty pos
    | MapValueEmpty pos
    | ListMemberEmpty pos
    | UnknownNotImplemented pos


errorToString : TransformError pos -> String
errorToString err =
    case err of
        UnresolvedRef _ hint ->
            hint ++ " reference did not resolve."

        NoMembers _ name ->
            name ++ ": structure has no members"

        MapKeyEmpty _ ->
            "Map .key is empty."

        MapValueEmpty _ ->
            "Map .value is empty."

        ListMemberEmpty _ ->
            "List .member is empty, but should be a shape reference."

        UnknownNotImplemented _ ->
            "Unknown not implemented."


transform : AWSService -> ResultME String (L3 ())
transform service =
    let
        mappingsResult : ResultME String (L1 ())
        mappingsResult =
            modelShapes service.shapes
                |> ResultME.mapError errorToString

        operationsResult : ResultME String (L1 ())
        operationsResult =
            modelOperations service.operations
                |> ResultME.mapError errorToString

        l2Result =
            Result.map2 List.append mappingsResult operationsResult
                |> ResultME.andThen
                    (Checker.check >> ResultME.mapError Checker.errorToString)
    in
    ResultME.map
        (\l2 ->
            { properties =
                Dict.empty
                    |> Dict.insert "name" (PQName [ "AWS", Case.toCamelCaseUpper service.metaData.serviceId ])
                    |> Dict.insert "isRegional" (PBool (Maybe.Extra.isNothing service.metaData.globalEndpoint))
                    |> Dict.insert "endpointPrefix" (PString service.metaData.endpointPrefix)
                    |> Dict.insert "apiVersion" (PString service.metaData.apiVersion)
                    |> Dict.insert "protocol"
                        (protocolToString service.metaData.protocol
                            |> PEnum AWSStubs.protocolEnum
                        )
                    |> Dict.insert "signer"
                        (service.metaData.signatureVersion
                            |> Maybe.withDefault SignV4
                            |> signerToString
                            |> PEnum AWSStubs.signerEnum
                        )
                    |> Dict.insert "xmlNamespace"
                        (Maybe.map PString service.metaData.xmlNamespace
                            |> POptional PSString
                        )
                    |> Dict.insert "targetPrefix"
                        (Maybe.map PString service.metaData.targetPrefix
                            |> POptional PSString
                        )
                    |> Dict.insert "signingName"
                        (Maybe.map PString service.metaData.signingName
                            |> POptional PSString
                        )
                    |> Dict.insert "jsonVersion"
                        (Maybe.map PString service.metaData.jsonVersion
                            |> POptional PSString
                        )
                    |> Dict.insert "documentation"
                        (Maybe.map PString service.documentation
                            |> POptional PSString
                        )
            , declarations = l2
            }
        )
        l2Result


protocolToString : Protocol -> String
protocolToString proto =
    case proto of
        EC2 ->
            "EC2"

        JSON ->
            "JSON"

        QUERY ->
            "QUERY"

        REST_JSON ->
            "REST_JSON"

        REST_XML ->
            "REST_XML"


signerToString : Signer -> String
signerToString signer =
    case signer of
        SignV4 ->
            "SignV4"

        SignS3 ->
            "SignS3"


httpMethodToString : HttpMethod -> String
httpMethodToString method =
    case method of
        DELETE ->
            "DELETE"

        GET ->
            "GET"

        HEAD ->
            "HEAD"

        OPTIONS ->
            "OPTIONS"

        POST ->
            "POST"

        PUT ->
            "PUT"


shapeRefToL1Type : ShapeRef -> Type () Unchecked
shapeRefToL1Type ref =
    TNamed () ref.shape Unchecked



--=== L1 Model Assembly Pass
-- A complete L1 model is generated for each shape.
-- Errors in the shape definitions are detected, but other checking of the L1
-- model is handled when it is lowered into L2.


modelShapes :
    Dict String Shape
    -> ResultME (TransformError ()) (L1 ())
modelShapes shapeDict =
    Dict.map
        (\key value -> modelShape value key)
        shapeDict
        |> ResultME.combineDict
        |> Result.map Dict.toList


modelShape : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelShape shape name =
    case shape.type_ of
        AString ->
            modelString shape name

        ABoolean ->
            DAlias () (BBool |> TBasic ()) L1.emptyProperties |> Ok

        AInteger ->
            modelInt shape name

        ALong ->
            DAlias () (BInt |> TBasic ()) L1.emptyProperties |> Ok

        AFloat ->
            DAlias () (BReal |> TBasic ()) L1.emptyProperties |> Ok

        ADouble ->
            DAlias () (BReal |> TBasic ()) L1.emptyProperties |> Ok

        ABlob ->
            DAlias () (BString |> TBasic ()) L1.emptyProperties |> Ok

        AStructure ->
            modelStructure shape name

        AList ->
            modelList shape name

        AMap ->
            modelMap shape name

        ATimestamp ->
            DAlias () (BString |> TBasic ()) L1.emptyProperties |> Ok

        AUnknown ->
            UnknownNotImplemented () |> ResultME.error


modelString : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
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
                    DEnum () nonemptyEnumVals L1.emptyProperties
                        |> Ok

                Nothing ->
                    NoMembers () name |> ResultME.error

        ( Nothing, True ) ->
            DRestricted
                ()
                (RString { minLength = shape.min, maxLength = shape.max, regex = shape.pattern })
                L1.emptyProperties
                |> Ok

        ( _, _ ) ->
            DAlias () (TBasic () BString) L1.emptyProperties |> Ok


modelInt : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            DRestricted ()
                (RInt { min = shape.min, max = shape.max, width = Nothing })
                L1.emptyProperties
                |> Ok

        _ ->
            DAlias () (BInt |> TBasic ()) L1.emptyProperties |> Ok


modelStructure : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
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
                    , ( memberName
                      , type_ |> COptional |> TContainer ()
                      , L1.emptyProperties
                      )
                        :: fieldAccum
                    )

                Just requiredFields ->
                    if List.member memberName requiredFields then
                        ( errAccum
                        , ( memberName, type_, L1.emptyProperties ) :: fieldAccum
                        )

                    else
                        ( errAccum
                        , ( memberName
                          , type_ |> COptional |> TContainer ()
                          , L1.emptyProperties
                          )
                            :: fieldAccum
                        )
    in
    case shape.members of
        Nothing ->
            NoMembers () name |> ResultME.error

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
                            let
                                product =
                                    nonemptyFields
                                        |> Naming.sortNonemptyNamed
                                        |> TProduct ()
                            in
                            DAlias () product L1.emptyProperties |> Ok

                        Nothing ->
                            DAlias () (TEmptyProduct ()) L1.emptyProperties |> Ok

                err :: errs ->
                    -- ResultME.errors err errs
                    Debug.todo "Fix error handling"


modelList : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelList shape name =
    case shape.member of
        Nothing ->
            ListMemberEmpty () |> ResultME.error

        Just ref ->
            DAlias () (shapeRefToL1Type ref |> CList |> TContainer ()) L1.emptyProperties |> Ok


modelMap : Shape -> String -> ResultME (TransformError ()) (Declarable () Unchecked)
modelMap shape name =
    let
        keyTypeRes =
            case shape.key of
                Nothing ->
                    MapKeyEmpty () |> ResultME.error

                Just keyRef ->
                    shapeRefToL1Type keyRef |> Ok

        valTypeRes =
            case shape.value of
                Nothing ->
                    MapValueEmpty () |> ResultME.error

                Just valRef ->
                    shapeRefToL1Type valRef |> Ok
    in
    ResultME.combine2
        (\keyType valType -> DAlias () (TContainer () (CDict keyType valType)) L1.emptyProperties)
        keyTypeRes
        valTypeRes



--== Operations


modelOperations :
    Dict String Operation
    -> ResultME (TransformError ()) (L1 ())
modelOperations operations =
    Dict.map
        (\name operation -> modelOperation name operation)
        operations
        |> ResultME.combineDict
        |> Result.map Dict.toList


modelOperation : String -> Operation -> ResultME (TransformError ()) (Declarable () Unchecked)
modelOperation name operation =
    let
        paramType opShapeRef errHint =
            case opShapeRef of
                Nothing ->
                    TUnit () |> Ok

                Just shapeRef ->
                    TNamed () shapeRef.shape Unchecked |> Ok

        requestRes =
            paramType operation.input "Input"

        responseRes =
            paramType operation.output "Output"
    in
    ResultME.combine2
        (\request response ->
            let
                funType =
                    TFunction () request response

                props =
                    Dict.empty
                        |> Dict.insert "url" (operation.http.requestUri |> Maybe.withDefault "/" |> PString)
                        |> Dict.insert "httpMethod" (httpMethodToString operation.http.method |> PString)
                        |> Dict.insert "documentation"
                            (Maybe.map PString operation.documentation
                                |> POptional PSString
                            )
            in
            DAlias () funType props
        )
        requestRes
        responseRes
