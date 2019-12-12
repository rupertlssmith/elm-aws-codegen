module Templates.Api exposing (coreServiceMod, docs, globalService, module_, regionalService, service, serviceFile)

import AWS.Core.Service exposing (Protocol(..), Signer(..))
import AWSApiModel exposing (AWSApiModel, Endpoint)
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Declaration, Expression, File, Import, Linkage, Module, Pattern, TopLevelExpose, TypeAnnotation)
import Enum
import HttpMethod exposing (HttpMethod)
import L1
import Maybe.Extra
import Templates.L1
import Templates.Util as Util


serviceFile : AWSApiModel -> File
serviceFile model =
    let
        ( serviceFn, linkage ) =
            service model

        ( endpoints, linkage2 ) =
            operations model

        ( types, linkage3 ) =
            typeDeclarations model

        ( codecs, linkage4 ) =
            jsonCodecs model

        declarations =
            codecs
                |> List.append types
                |> List.append endpoints
                |> (::) serviceFn

        linkages =
            [ linkage, linkage2, linkage3, linkage4 ]

        ( imports, exposings ) =
            CG.combineLinkage linkages

        moduleSpec =
            module_ model exposings
    in
    CG.file moduleSpec imports declarations Nothing


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



--== Module Specification (with exposing).


module_ : AWSApiModel -> List TopLevelExpose -> Module
module_ model exposings =
    CG.normalModule model.name exposings



--== Module Documentation
-- {-| {{= it.documentation }}
--
-- @docs service
--
-- ## Table of Contents
--
-- * [Operations](#operations){{~ it.categories :c }}
-- * [{{= c.name }}](#{{= c.name.toLowerCase() }}){{~}}
--
-- ## Operations
--
-- {{~ it.operationNames :name }}* [{{= name }}](#{{= name }})
-- {{~}}
--
-- @docs {{= it.operationNames.join(',') }}
-- {{~ it.categories :c }}
-- ## {{= c.name }}
--
-- {{~ c.types.filter(t => t.exposeAs).map(t => t.type) :t }}* [{{= t }}](#{{= t }})
-- {{~}}
--
-- @docs {{= c.types.filter(t => t.exposeAs).map(t => t.type).join(',') }}
-- {{~}}
-- -}


docs =
    ""



--== Service Definition


service : AWSApiModel -> ( Declaration, Linkage )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


optionsFn model =
    let
        jsonVersionOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setJsonVersion", CG.string name ])
                model.jsonVersion

        signingNameOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setSigningName", CG.string name ])
                model.signingName

        targetPrefixOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setTargetPrefix", CG.string name ])
                model.targetPrefix

        xmlNamespaceOption =
            Maybe.map
                (\name -> CG.apply [ CG.fqFun coreServiceMod "setXmlNamespace", CG.string name ])
                model.xmlNamespace

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


regionalService : AWSApiModel -> ( Declaration, Linkage )
regionalService model =
    let
        sig =
            CG.funAnn
                (CG.fqTyped coreServiceMod "Region" [])
                (CG.fqTyped coreServiceMod "Service" [])

        impl =
            CG.apply
                [ CG.fqFun coreServiceMod "defineRegional"
                , CG.string model.endpointPrefix
                , CG.string model.apiVersion
                , protocolExpr model.protocol
                , signerExpr model.signer
                , CG.fun "optionsFn"
                ]
                |> CG.letExpr [ optionsFn model ]

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


globalService : AWSApiModel -> ( Declaration, Linkage )
globalService model =
    let
        sig =
            CG.fqTyped coreServiceMod "Service" []

        impl =
            CG.apply
                [ CG.fqFun coreServiceMod "defineGlobal"
                , CG.string model.endpointPrefix
                , CG.string model.apiVersion
                , protocolExpr model.protocol
                , signerExpr model.signer
                , CG.fun "optionsFn"
                ]
                |> CG.letExpr [ optionsFn model ]

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



--== Operations


operations : AWSApiModel -> ( List Declaration, Linkage )
operations model =
    Dict.foldl
        (\name operation ( declAccum, linkageAccum ) ->
            requestFn name operation
                |> Tuple.mapFirst (\decl -> decl :: declAccum)
                |> Tuple.mapSecond (\linkage -> CG.combineLinkage [ linkageAccum, linkage ])
        )
        ( [], CG.emptyLinkage )
        model.operations


requestFn : String -> Endpoint -> ( Declaration, Linkage )
requestFn name op =
    let
        { maybeRequestType, argPatterns, jsonBody, requestLinkage } =
            requestFnRequest name op

        ( responseType, responseDecoder, responseLinkage ) =
            requestFnResponse name op

        wrappedResponseType =
            CG.fqTyped coreHttpMod "Request" [ responseType ]

        requestSig =
            case maybeRequestType of
                Just requestType ->
                    CG.funAnn requestType wrappedResponseType

                Nothing ->
                    wrappedResponseType

        requestImpl =
            CG.apply
                [ CG.fqFun coreHttpMod "request"
                , CG.string (Util.safeCCU name)
                , CG.fqVal coreHttpMod (Enum.toString HttpMethod.httpMethodEnum op.httpMethod)
                , CG.string op.url
                , CG.val "jsonBody"
                , CG.val "decoder"
                ]
                |> CG.letExpr
                    [ jsonBody |> CG.letVal "jsonBody"
                    , responseDecoder |> CG.letVal "decoder"
                    ]

        doc =
            CG.emptyDocComment
                |> CG.markdown "AWS Endpoint."
    in
    ( CG.funDecl
        (Just doc)
        (Just requestSig)
        (Util.safeCCL name)
        argPatterns
        requestImpl
    , CG.combineLinkage
        [ requestLinkage
        , responseLinkage
        , CG.emptyLinkage
            |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
            |> CG.addExposing (CG.funExpose (Util.safeCCL name))
        ]
    )


{-| Figures out what the request type for the endpoint will be.

If there is no request type defined for the endpoint then 'Nothing' will be returned,
and an empty JSON body expression will be given.

The output of this is the optional request type alias, a list of patterns for the
request functions arguments, the json body and any linkage that needs to be rolled up.

-}
requestFnRequest :
    String
    -> Endpoint
    ->
        { maybeRequestType : Maybe TypeAnnotation
        , argPatterns : List Pattern
        , jsonBody : Expression
        , requestLinkage : Linkage
        }
requestFnRequest name op =
    case op.request of
        (L1.TNamed requestTypeName _) as l1RequestType ->
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
requestFnResponse : String -> Endpoint -> ( TypeAnnotation, Expression, Linkage )
requestFnResponse name op =
    case op.response of
        (L1.TNamed responseTypeName _) as l1ResponseType ->
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


typeDeclarations : AWSApiModel -> ( List Declaration, Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl ( declAccum, linkageAccum ) ->
            Templates.L1.typeDecl name decl
                |> Tuple.mapFirst (List.append declAccum)
                |> Tuple.mapSecond (\innerLinkage -> CG.combineLinkage [ linkageAccum, innerLinkage ])
        )
        ( [], CG.emptyLinkage )
        model.declarations


jsonCodecs : AWSApiModel -> ( List Declaration, Linkage )
jsonCodecs model =
    Dict.foldl
        (\name decl accum -> Templates.L1.codec name decl :: accum)
        []
        model.declarations
        |> List.unzip
        |> Tuple.mapSecond CG.combineLinkage



-- {{? t.toStringDef }}
-- {{= t.toStringDef }}
-- {{?}}
-- {{~}}


toParams =
    ()



-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}


requests =
    ()



-- Helpers


signerExpr : Signer -> Expression
signerExpr signer =
    case signer of
        SignV4 ->
            CG.fqVal coreServiceMod "SignV4"

        SignS3 ->
            CG.fqVal coreServiceMod "SignS3"


protocolExpr : Protocol -> Expression
protocolExpr protocol =
    case protocol of
        EC2 ->
            CG.fqVal coreServiceMod "EC2"

        JSON ->
            CG.fqVal coreServiceMod "JSON"

        QUERY ->
            CG.fqVal coreServiceMod "QUERY"

        REST_JSON ->
            CG.fqVal coreServiceMod "REST_JSON"

        REST_XML ->
            CG.fqVal coreServiceMod "REST_XML"


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


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing Nothing
