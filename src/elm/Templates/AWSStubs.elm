module Templates.AWSStubs exposing (AWSStubsError(..), check, defaultProperties, errorToString, generate, generator)

import AWS.Core.Service exposing (Protocol(..), Signer(..))
import Dict
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Import, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Enum exposing (Enum)
import HttpMethod exposing (HttpMethod)
import L1 exposing (PropSpec(..), Properties, Property(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, L3, Processor)
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
            , ( "xmlNamespace", PSOptional PSString )
            , ( "targetPrefix", PSOptional PSString )
            , ( "signingName", PSOptional PSString )
            , ( "jsonVersion", PSOptional PSString )
            , ( "documentation", PSOptional PSString )
            ]
            [ ( "isRegional", PBool False )
            , ( "apiVersion", PString "1.0" )
            , ( "protocol", PEnum protocolEnum "JSON" )
            , ( "signer", PEnum signerEnum "V4" )
            ]
    , alias =
        L1.defineProperties
            [ ( "exclude", PBool False ) ]
            []
    , sum = L1.defineProperties [] []
    , enum =
        L1.defineProperties
            []
            [ ( "elmEnumStyle", PEnum elmEnumStyleEnum "customType" ) ]
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
errorToString =
    Debug.todo "errorToString"


check : L3 pos -> ResultME err (L3 pos)
check =
    Debug.todo "check"



--generate : L3 pos -> ResultME String File


generate : L3 pos -> ResultME L3.PropCheckError File
generate model =
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

                -- doc =
                --     documentation
                --         |> Maybe.withDefault CG.emptyFileComment
                --         |> CG.markdown "# Service definition."
                --         |> CG.docTagsFromExposings (Tuple.second serviceLinkage)
                --         |> CG.markdown "# Service endpoints."
                --         |> CG.docTagsFromExposings (Tuple.second operationsLinkage)
                --         |> CG.markdown "# API data model."
                --         |> CG.docTagsFromExposings (Tuple.second typeDeclLinkage)
                --         |> CG.markdown "# Codecs for the data model."
                --         |> CG.docTagsFromExposings (Tuple.second codecsLinkage)
            in
            -- CG.file moduleSpec imports declarations (Just doc)
            module_ model exposings
                |> ResultME.map (\moduleSpec -> CG.file moduleSpec imports declarations Nothing)
        )
        (service model)
        (operations model)
        (typeDeclarations model)
        (jsonCodecs model)
        (L3.getOptionalStringProperty "documentation" model.properties)
        |> ResultME.flatten



--== Module Specification (with exposing).


module_ : L3 pos -> List TopLevelExpose -> ResultME L3.PropCheckError Module
module_ model exposings =
    L3.getQNameProperty "name" model.properties
        |> ResultME.map (\( path, name ) -> CG.normalModule (name :: path) exposings)



--== Service Definition


service : L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
service model =
    L3.getBoolProperty "isRegional" model.properties
        |> ResultME.andThen
            (\isRegional ->
                if isRegional then
                    regionalService model

                else
                    globalService model
            )


{-| optionsFn : L3 pos -> LetDeclaration
-}
optionsFn model =
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
        (L3.getOptionalStringProperty "jsonVersion" model.properties)
        (L3.getOptionalStringProperty "signingName" model.properties)
        (L3.getOptionalStringProperty "targetPrefix" model.properties)
        (L3.getOptionalStringProperty "xmlNamespace" model.properties)


regionalService : L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
regionalService model =
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
        (L3.getStringProperty "endpointPrefix" model.properties)
        (L3.getStringProperty "apiVersion" model.properties)
        (L3.getEnumProperty protocolEnum "protocol" model.properties)
        (L3.getEnumProperty signerEnum "signer" model.properties)
        (optionsFn model)


globalService : L3 pos -> ResultME L3.PropCheckError ( Declaration, Linkage )
globalService model =
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
        (L3.getStringProperty "endpointPrefix" model.properties)
        (L3.getStringProperty "apiVersion" model.properties)
        (L3.getEnumProperty protocolEnum "protocol" model.properties)
        (L3.getEnumProperty signerEnum "signer" model.properties)
        (optionsFn model)



--== Operations


operations : L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
operations model =
    -- Dict.foldl
    --     (\name operation ( declAccum, linkageAccum ) ->
    --         requestFn name operation
    --             |> Tuple.mapFirst (\decl -> decl :: declAccum)
    --             |> Tuple.mapSecond (\linkage -> CG.combineLinkage [ linkageAccum, linkage ])
    --     )
    --     ( [], CG.emptyLinkage )
    --     model.operations
    ( [], CG.emptyLinkage ) |> Ok


requestFn :
    String
    -> Properties
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> ( Declaration, Linkage )
requestFn name props pos request response =
    -- let
    --     { maybeRequestType, argPatterns, jsonBody, requestLinkage } =
    --         requestFnRequest name op
    --
    --     ( responseType, responseDecoder, responseLinkage ) =
    --         requestFnResponse name op
    --
    --     wrappedResponseType =
    --         CG.fqTyped coreHttpMod "Request" [ responseType ]
    --
    --     requestSig =
    --         case maybeRequestType of
    --             Just requestType ->
    --                 CG.funAnn requestType wrappedResponseType
    --
    --             Nothing ->
    --                 wrappedResponseType
    --
    --     requestImpl =
    --         CG.apply
    --             [ CG.fqFun coreHttpMod "request"
    --             , CG.string (Util.safeCCU name)
    --             , CG.fqVal coreHttpMod (Enum.toString HttpMethod.httpMethodEnum op.httpMethod)
    --             , CG.string op.url
    --             , CG.val "jsonBody"
    --             , CG.val "decoder"
    --             ]
    --             |> CG.letExpr
    --                 [ jsonBody |> CG.letVal "jsonBody"
    --                 , responseDecoder |> CG.letVal "decoder"
    --                 ]
    --
    --     doc =
    --         op.documentation
    --             |> Maybe.withDefault CG.emptyDocComment
    -- in
    -- ( CG.funDecl
    --     (Just doc)
    --     (Just requestSig)
    --     (Util.safeCCL name)
    --     argPatterns
    --     requestImpl
    -- , CG.combineLinkage
    --     [ requestLinkage
    --     , responseLinkage
    --     , CG.emptyLinkage
    --         |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
    --         |> CG.addExposing (CG.funExpose (Util.safeCCL name))
    --     ]
    -- )
    ( CG.portDecl "dummy" CG.unitAnn, CG.emptyLinkage )


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-}
requestFnRequest :
    String
    -> Properties
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    ->
        { maybeRequestType : Maybe TypeAnnotation
        , argPatterns : List Pattern
        , jsonBody : Expression
        , requestLinkage : Linkage
        }
requestFnRequest name props pos request response =
    -- case op.request of
    --     (L1.TNamed _ requestTypeName _) as l1RequestType ->
    --         let
    --             ( loweredType, loweredLinkage ) =
    --                 Templates.L1.lowerType l1RequestType
    --
    --             linkage =
    --                 CG.combineLinkage
    --                     [ CG.emptyLinkage
    --                         |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
    --                     , loweredLinkage
    --                     ]
    --
    --             jsonBody =
    --                 CG.pipe (CG.val "req")
    --                     [ CG.apply
    --                         [ CG.fqFun codecMod "encoder"
    --                         , CG.val (Util.safeCCL requestTypeName ++ "Codec")
    --                         ]
    --                     , CG.fqVal coreHttpMod "jsonBody"
    --                     ]
    --         in
    --         { maybeRequestType = Just loweredType
    --         , argPatterns = [ CG.varPattern "req" ]
    --         , jsonBody = jsonBody
    --         , requestLinkage = linkage
    --         }
    --
    --     _ ->
    --         let
    --             emptyJsonBody =
    --                 CG.fqVal coreHttpMod "emptyBody"
    --
    --             linkage =
    --                 CG.emptyLinkage |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
    --         in
    --         { maybeRequestType = Nothing
    --         , argPatterns = []
    --         , jsonBody = emptyJsonBody
    --         , requestLinkage = linkage
    --         }
    { maybeRequestType = Nothing
    , argPatterns = []
    , jsonBody = CG.unit
    , requestLinkage = CG.emptyLinkage
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
    -> Properties
    -> pos
    -> L1.Type pos L2.RefChecked
    -> L1.Type pos L2.RefChecked
    -> ( TypeAnnotation, Expression, Linkage )
requestFnResponse name props pos request response =
    -- case op.response of
    --     (L1.TNamed _ responseTypeName _) as l1ResponseType ->
    --         let
    --             ( loweredType, loweredLinkage ) =
    --                 Templates.L1.lowerType l1ResponseType
    --
    --             responseType =
    --                 loweredType
    --
    --             linkage =
    --                 CG.combineLinkage
    --                     [ CG.emptyLinkage
    --                         |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
    --                     , loweredLinkage
    --                     ]
    --
    --             decoder =
    --                 CG.apply
    --                     [ CG.fqFun codecMod "decoder"
    --                     , CG.val (Util.safeCCL responseTypeName ++ "Codec")
    --                     ]
    --                     |> CG.parens
    --         in
    --         ( responseType, decoder, linkage )
    --
    --     _ ->
    --         let
    --             linkage =
    --                 CG.emptyLinkage
    --                     |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)
    --                     |> CG.addImport decodeImport
    --
    --             decoder =
    --                 CG.apply
    --                     [ CG.fqVal decodeMod "succeed"
    --                     , CG.unit
    --                     ]
    --
    --             responseType =
    --                 CG.unitAnn
    --         in
    --         ( responseType, decoder, linkage )
    ( CG.unitAnn, CG.unit, CG.emptyLinkage )



--== Types and Codecs


typeDeclarations : L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl ( declAccum, linkageAccum ) ->
            let
                doc =
                    CG.emptyDocComment
                        |> CG.markdown ("The " ++ Util.safeCCU name ++ " data model.")
            in
            L3.getBoolProperty "exclude" model.properties
                |> ResultME.map
                    (\stubGen ->
                        case stubGen of
                            Just "model" ->
                                Templates.L1.typeDecl name doc decl
                                    |> Tuple.mapFirst (List.append declAccum)
                                    |> Tuple.mapSecond (\innerLinkage -> CG.combineLinkage [ linkageAccum, innerLinkage ])

                            _ ->
                                ( declAccum, linkageAccum )
                    )
        )
        ( [], CG.emptyLinkage )
        model.declarations


jsonCodecs : L3 pos -> ResultME L3.PropCheckError ( List Declaration, Linkage )
jsonCodecs model =
    -- Dict.foldl
    --     (\name decl accum -> Templates.L1.codec name decl :: accum)
    --     []
    --     model.declarations
    --     |> List.unzip
    --     |> Tuple.mapSecond CG.combineLinkage
    ( [], CG.emptyLinkage ) |> Ok



-- Helpers
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
