module Templates.AWSStubs exposing
    ( AWSStubsError(..)
    , check
    , defaultProperties
    , elmEnumStyleEnum
    , errorToString
    , generate
    , generator
    , protocolEnum
    , signerEnum
    )

import AWS.Core.Service exposing (Protocol(..), Signer(..))
import Dict
import Documentation
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Import, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Enum exposing (Enum)
import HttpMethod exposing (HttpMethod)
import L1 exposing (Declarable(..), PropSpec(..), Properties, Property(..), Type(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, L3, Processor, PropertiesAPI)
import Maybe.Extra
import ResultME exposing (ResultME)
import Templates.L1
import Templates.Util as Util


protocolEnum : Enum String
protocolEnum =
    Enum.define
        [ "EC2"
        , "JSON"
        , "QUERY"
        , "REST_JSON"
        , "REST_XML"
        ]
        identity


signerEnum : Enum String
signerEnum =
    Enum.define
        [ "SignS3"
        , "SignV4"
        ]
        identity


elmEnumStyleEnum : Enum String
elmEnumStyleEnum =
    Enum.define
        [ "customType"
        , "guardedType"
        , "listOfStrings"
        ]
        identity


defaultProperties : DefaultProperties
defaultProperties =
    { top =
        L1.defineProperties
            [ ( "name", PSQName )
            , ( "endpointPrefix", PSString )
            , ( "apiVersion", PSString )
            , ( "protocol", PSEnum protocolEnum )
            , ( "signer", PSEnum signerEnum )
            ]
            [ ( "isRegional", PBool False )
            , ( "xmlNamespace", POptional PSString Nothing )
            , ( "targetPrefix", POptional PSString Nothing )
            , ( "signingName", POptional PSString Nothing )
            , ( "jsonVersion", POptional PSString Nothing )
            , ( "documentation", POptional PSString Nothing )
            ]
    , alias =
        L1.defineProperties
            [ ( "url", PSString ) -- TODO: Put these on the function.
            , ( "httpMethod", PSString ) -- TODO: Put these on the function.
            ]
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            ]
    , sum =
        L1.defineProperties []
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            ]
    , enum =
        L1.defineProperties
            []
            [ ( "exclude", PBool False )
            , ( "elmEnumStyle", PEnum elmEnumStyleEnum "customType" )
            , ( "documentation", POptional PSString Nothing )
            ]
    , restricted =
        L1.defineProperties
            []
            [ ( "exclude", PBool False )
            , ( "documentation", POptional PSString Nothing )
            ]
    , fields = L1.defineProperties [] []
    }


generator : Processor pos AWSStubsError
generator =
    { name = "AWSStubs"
    , defaults = defaultProperties
    , check = check
    , errorToString = errorToString
    }


type AWSStubsError
    = AWSStubsError


errorToString : (pos -> String) -> pos -> err -> String
errorToString _ _ _ =
    "errorToString"


check : L3 pos -> ResultME err (L3 pos)
check l3 =
    -- Debug.todo "check"
    l3 |> Ok



--generate : L3 pos -> ResultME String File


generate : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError File
generate propertiesApi model =
    ResultME.combine5
        (\( serviceFn, serviceLinkage ) ( endpoints, operationsLinkage ) ( types, typeDeclLinkage ) ( codecs, codecsLinkage ) documentation ->
            let
                declarations =
                    codecs
                        |> List.append types
                        |> List.append endpoints
                        |> (::) serviceFn

                linkages =
                    [ serviceLinkage, operationsLinkage, typeDeclLinkage, codecsLinkage ]

                ( imports, exposings ) =
                    CG.combineLinkage linkages

                doc =
                    documentation
                        |> Maybe.map Documentation.htmlToFileComment
                        |> Maybe.withDefault CG.emptyFileComment
                        |> CG.markdown "# Service definition."
                        |> CG.docTagsFromExposings (Tuple.second serviceLinkage)
                        |> CG.markdown "# Service endpoints."
                        |> CG.docTagsFromExposings (Tuple.second operationsLinkage)
                        |> CG.markdown "# API data model."
                        |> CG.docTagsFromExposings (Tuple.second typeDeclLinkage)
                        |> CG.markdown "# Codecs for the data model."
                        |> CG.docTagsFromExposings (Tuple.second codecsLinkage)
            in
            module_ propertiesApi model exposings
                |> ResultME.map (\moduleSpec -> CG.file moduleSpec imports declarations (Just doc))
        )
        (service propertiesApi model)
        (operations propertiesApi model)
        (typeDeclarations propertiesApi model)
        (jsonCodecs propertiesApi model)
        (propertiesApi.top.getOptionalStringProperty "documentation")
        |> ResultME.flatten



--== Module Specification (with exposing).


module_ : PropertiesAPI pos -> L3 pos -> List TopLevelExpose -> ResultME L3.PropCheckError Module
module_ propertiesApi model exposings =
    propertiesApi.top.getQNameProperty "name"
        |> ResultME.map (\path -> CG.normalModule path exposings)



--== Service Definition


service : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
service propertiesApi model =
    propertiesApi.top.getBoolProperty "isRegional"
        |> ResultME.andThen
            (\isRegional ->
                if isRegional then
                    regionalService propertiesApi model

                else
                    globalService propertiesApi model
            )


{-| optionsFn : L3 pos -> LetDeclaration
-}
optionsFn propertiesApi model =
    ResultME.combine4
        (\jsonVersion signingName targetPrefix xmlNamespace ->
            let
                jsonVersionOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setJsonVersion", CG.string name ])
                        jsonVersion

                signingNameOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setSigningName", CG.string name ])
                        signingName

                targetPrefixOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setTargetPrefix", CG.string name ])
                        targetPrefix

                xmlNamespaceOption =
                    Maybe.map
                        (\name -> CG.apply [ CG.fqFun coreServiceMod "setXmlNamespace", CG.string name ])
                        xmlNamespace

                options =
                    [ jsonVersionOption, signingNameOption, targetPrefixOption, xmlNamespaceOption ] |> Maybe.Extra.values
            in
            (case options of
                [] ->
                    CG.fun "identity"

                op :: ops ->
                    CG.chain op (List.map CG.parens ops)
            )
                |> CG.letFunction "optionsFn" []
        )
        (propertiesApi.top.getOptionalStringProperty "jsonVersion")
        (propertiesApi.top.getOptionalStringProperty "signingName")
        (propertiesApi.top.getOptionalStringProperty "targetPrefix")
        (propertiesApi.top.getOptionalStringProperty "xmlNamespace")


regionalService : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
regionalService propertiesApi model =
    ResultME.combine5
        (\endpointPrefix apiVersion protocol signer options ->
            let
                sig =
                    CG.funAnn
                        (CG.fqTyped coreServiceMod "Region" [])
                        (CG.fqTyped coreServiceMod "Service" [])

                impl =
                    CG.apply
                        [ CG.fqFun coreServiceMod "defineRegional"
                        , CG.string endpointPrefix
                        , CG.string apiVersion
                        , CG.fqVal coreServiceMod protocol
                        , CG.fqVal coreServiceMod signer
                        , CG.fun "optionsFn"
                        ]
                        |> CG.letExpr [ options ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown "Configuration for this service."
            in
            ( CG.funDecl
                (Just doc)
                (Just sig)
                "service"
                []
                impl
            , CG.emptyLinkage
                |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
                |> CG.addExposing (CG.funExpose "service")
            )
        )
        (propertiesApi.top.getStringProperty "endpointPrefix")
        (propertiesApi.top.getStringProperty "apiVersion")
        (propertiesApi.top.getEnumProperty protocolEnum "protocol")
        (propertiesApi.top.getEnumProperty signerEnum "signer")
        (optionsFn propertiesApi model)


globalService : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
globalService propertiesApi model =
    ResultME.combine5
        (\endpointPrefix apiVersion protocol signer options ->
            let
                sig =
                    CG.fqTyped coreServiceMod "Service" []

                impl =
                    CG.apply
                        [ CG.fqFun coreServiceMod "defineGlobal"
                        , CG.string endpointPrefix
                        , CG.string apiVersion
                        , CG.fqVal coreServiceMod protocol
                        , CG.fqVal coreServiceMod signer
                        , CG.fun "optionsFn"
                        ]
                        |> CG.letExpr [ options ]

                doc =
                    CG.emptyDocComment
                        |> CG.markdown "Configuration for this service."
            in
            ( CG.funDecl
                (Just doc)
                (Just sig)
                "service"
                []
                impl
            , CG.emptyLinkage
                |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
                |> CG.addExposing (CG.funExpose "service")
            )
        )
        (propertiesApi.top.getStringProperty "endpointPrefix")
        (propertiesApi.top.getStringProperty "apiVersion")
        (propertiesApi.top.getEnumProperty protocolEnum "protocol")
        (propertiesApi.top.getEnumProperty signerEnum "signer")
        (optionsFn propertiesApi model)



--== Operations


operations : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
operations propertiesApi model =
    declarationsSkipExcluded propertiesApi (operation propertiesApi) model
        |> ResultME.map ResultME.combineList
        |> ResultME.flatten
        |> ResultME.map combineDeclarations


operation :
    PropertiesAPI pos
    -> String
    -> L1.Declarable pos L2.RefChecked
    -> ResultME L3.PropCheckError ( List Declaration, Linkage )
operation propertiesApi name decl =
    case decl of
        DAlias pos (TFunction _ request response) _ ->
            requestFn (propertiesApi.declarable decl) name pos request response

        _ ->
            ( [], CG.emptyLinkage ) |> Ok


requestFn :
    L3.PropertyGet
    -> String
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> ResultME L3.PropCheckError ( List Declaration, Linkage )
requestFn propertyGet name pos request response =
    let
        { maybeRequestType, argPatterns, jsonBody, requestLinkage } =
            requestFnRequest name request

        ( responseType, responseDecoder, responseLinkage ) =
            requestFnResponse name response

        wrappedResponseType =
            CG.fqTyped coreHttpMod "Request" [ responseType ]

        requestSig =
            case maybeRequestType of
                Just requestType ->
                    CG.funAnn requestType wrappedResponseType

                Nothing ->
                    wrappedResponseType
    in
    ResultME.combine3
        (\url httpMethod documentation ->
            let
                requestImpl =
                    CG.apply
                        [ CG.fqFun coreHttpMod "request"
                        , CG.string (Util.safeCCU name)
                        , CG.fqVal coreHttpMod httpMethod
                        , CG.string url
                        , CG.val "jsonBody"
                        , CG.val "decoder"
                        ]
                        |> CG.letExpr
                            [ jsonBody |> CG.letVal "jsonBody"
                            , responseDecoder |> CG.letVal "decoder"
                            ]

                doc =
                    documentation
                        |> Maybe.map Documentation.htmlToDocComment
                        |> Maybe.withDefault CG.emptyDocComment
            in
            ( [ CG.funDecl
                    (Just doc)
                    (Just requestSig)
                    (Util.safeCCL name)
                    argPatterns
                    requestImpl
              ]
            , CG.combineLinkage
                [ requestLinkage
                , responseLinkage
                , CG.emptyLinkage
                    |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                    |> CG.addExposing (CG.funExpose (Util.safeCCL name))
                ]
            )
        )
        (propertyGet.getStringProperty "url")
        (propertyGet.getStringProperty "httpMethod")
        (propertyGet.getOptionalStringProperty "documentation")


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-}
requestFnRequest :
    String
    -> L1.Type pos L2.RefChecked
    ->
        { maybeRequestType : Maybe TypeAnnotation
        , argPatterns : List Pattern
        , jsonBody : Expression
        , requestLinkage : Linkage
        }
requestFnRequest name request =
    case request of
        (L1.TNamed _ requestTypeName _) as l1RequestType ->
            let
                ( loweredType, loweredLinkage ) =
                    Templates.L1.lowerType l1RequestType

                linkage =
                    CG.combineLinkage
                        [ CG.emptyLinkage
                            |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                        , loweredLinkage
                        ]

                jsonBody =
                    CG.pipe (CG.val "req")
                        [ CG.apply
                            [ CG.fqFun codecMod "encoder"
                            , CG.val (Util.safeCCL requestTypeName ++ "Codec")
                            ]
                        , CG.fqVal coreHttpMod "jsonBody"
                        ]
            in
            { maybeRequestType = Just loweredType
            , argPatterns = [ CG.varPattern "req" ]
            , jsonBody = jsonBody
            , requestLinkage = linkage
            }

        _ ->
            let
                emptyJsonBody =
                    CG.fqVal coreHttpMod "emptyBody"

                linkage =
                    CG.emptyLinkage |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
            in
            { maybeRequestType = Nothing
            , argPatterns = []
            , jsonBody = emptyJsonBody
            , requestLinkage = linkage
            }


{-| Figures out what response type for the endpoint will be.

If there is no response type defined for the endpoint then `()` is used to indicate
that the response has completed but returned no data.

The output of this is the response type alias for the endpoint, the decoder for this
expected response and any linkage that needs to be rolled up.

When there is no response shape, the decoder will be `(AWS.Core.Decode.FixedResult ()`.

-}
requestFnResponse :
    String
    -> L1.Type pos L2.RefChecked
    -> ( TypeAnnotation, Expression, Linkage )
requestFnResponse name response =
    case response of
        (L1.TNamed _ responseTypeName _) as l1ResponseType ->
            let
                ( loweredType, loweredLinkage ) =
                    Templates.L1.lowerType l1ResponseType

                responseType =
                    loweredType

                linkage =
                    CG.combineLinkage
                        [ CG.emptyLinkage
                            |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
                        , loweredLinkage
                        ]

                decoder =
                    CG.apply
                        [ CG.fqFun codecMod "decoder"
                        , CG.val (Util.safeCCL responseTypeName ++ "Codec")
                        ]
                        |> CG.parens
            in
            ( responseType, decoder, linkage )

        _ ->
            let
                linkage =
                    CG.emptyLinkage
                        |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
                        |> CG.addImport decodeImport

                decoder =
                    CG.apply
                        [ CG.fqVal decodeMod "succeed"
                        , CG.unit
                        ]

                responseType =
                    CG.unitAnn
            in
            ( responseType, decoder, linkage )



--== Types and Codecs


typeDeclarations : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
typeDeclarations propertiesAPI model =
    declarationsSkipExcluded propertiesAPI typeDeclaration model
        |> ResultME.map combineDeclarations


typeDeclaration : String -> L1.Declarable pos L2.RefChecked -> ( List Declaration, Linkage )
typeDeclaration name decl =
    case decl of
        DAlias _ (TFunction _ _ _) _ ->
            let
                _ =
                    Debug.log "typeDeclaration" ("Skipped function " ++ name)
            in
            ( [], CG.emptyLinkage )

        _ ->
            let
                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("The " ++ Util.safeCCU name ++ " data model.")
            in
            Templates.L1.typeDecl name doc decl


jsonCodecs : PropertiesAPI pos -> L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
jsonCodecs propertiesAPI model =
    declarationsSkipExcluded propertiesAPI jsonCodec model
        |> ResultME.map combineDeclarations


jsonCodec : String -> L1.Declarable pos L2.RefChecked -> ( List Declaration, Linkage )
jsonCodec name decl =
    case decl of
        DAlias _ (TFunction _ _ _) _ ->
            let
                _ =
                    Debug.log "typeDeclaration" ("Skipped function " ++ name)
            in
            ( [], CG.emptyLinkage )

        _ ->
            Templates.L1.codec name decl
                |> Tuple.mapFirst List.singleton



-- Helpers


{-| Combines linkages from a list of declarations and linkages, into a list of declarations
and a single combined linkage.
-}
combineDeclarations : List ( List Declaration, Linkage ) -> ( List Declaration, Linkage )
combineDeclarations declList =
    List.foldl
        (\( decls, linkage ) ( declAccum, linkageAccum ) ->
            ( List.append declAccum decls, CG.combineLinkage [ linkageAccum, linkage ] )
        )
        ( [], CG.emptyLinkage )
        declList


{-| Combines linkages from a list of single declarations and linkages, into a
list of declarations and a single combined linkage.
-}
combineDeclaration : List ( Declaration, Linkage ) -> ( List Declaration, Linkage )
combineDeclaration declList =
    List.foldl
        (\( decl, linkage ) ( declAccum, linkageAccum ) ->
            ( decl :: declAccum, CG.combineLinkage [ linkageAccum, linkage ] )
        )
        ( [], CG.emptyLinkage )
        declList


{-| Runs a function to generate from all Declarables in a model, skipping any that
are marked as 'excluded'. Any errors are accumulated.
-}
declarationsSkipExcluded :
    PropertiesAPI pos
    -> (String -> L1.Declarable pos L2.RefChecked -> a)
    -> L3 pos
    -> ResultME L3.PropCheckError (List a)
declarationsSkipExcluded propertiesAPI declarationsFn model =
    Dict.foldl
        (\name decl accum ->
            case (propertiesAPI.declarable decl).getBoolProperty "exclude" of
                Ok False ->
                    (declarationsFn name decl |> Ok) :: accum

                Ok True ->
                    accum

                Err err ->
                    Err err :: accum
        )
        []
        model.declarations
        |> ResultME.combineList



-- signerExpr : Signer -> Expression
-- signerExpr signer =
--     case signer of
--         SignV4 ->
--             CG.fqVal coreServiceMod "SignV4"
--
--         SignS3 ->
--             CG.fqVal coreServiceMod "SignS3"
--
--
-- protocolExpr : Protocol -> Expression
-- protocolExpr protocol =
--     case protocol of
--         EC2 ->
--             CG.fqVal coreServiceMod "EC2"
--
--         JSON ->
--             CG.fqVal coreServiceMod "JSON"
--
--         QUERY ->
--             CG.fqVal coreServiceMod "QUERY"
--
--         REST_JSON ->
--             CG.fqVal coreServiceMod "REST_JSON"
--
--         REST_XML ->
--             CG.fqVal coreServiceMod "REST_XML"


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


codecMod : List String
codecMod =
    [ "Codec" ]


coreHttpMod : List String
coreHttpMod =
    [ "AWS", "Core", "Http" ]


coreDecodeMod : List String
coreDecodeMod =
    [ "AWS", "Core", "Decode" ]


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing Nothing
