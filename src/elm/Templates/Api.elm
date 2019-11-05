module Templates.Api exposing (coreServiceMod, docs, globalService, module_, regionalService, service, serviceFile)

import AWSApiModel exposing (AWSApiModel, Endpoint)
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Declaration, File, Linkage, Module, TopLevelExpose)
import L1
import Templates.L1


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
            linkage4
                |> List.append linkage3
                |> List.append linkage2
                |> (::) linkage

        ( imports, exposings ) =
            CG.combineLinkage linkages

        moduleSpec =
            module_ model exposings
    in
    CG.file moduleSpec imports declarations []


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
                , CG.fqFun coreServiceMod model.protocol
                , CG.fqFun coreServiceMod model.signer
                ]
    in
    ( CG.funDecl
        (Just "{-| Configuration for this service. -}")
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
                , CG.fqFun coreServiceMod model.protocol
                , CG.fqFun coreServiceMod model.signer
                ]
    in
    ( CG.funDecl
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport (CG.importStmt coreServiceMod Nothing Nothing)
        |> CG.addExposing (CG.funExpose "service")
    )



--== Operations


operations : AWSApiModel -> ( List Declaration, List Linkage )
operations model =
    Dict.foldl
        (\name operation ( declAccum, linkageAccum ) ->
            requestFn name operation
                |> Tuple.mapFirst (\decl -> decl :: declAccum)
                |> Tuple.mapSecond (\linkage -> linkage :: linkageAccum)
        )
        ( [], [] )
        model.operations


requestFn : String -> Endpoint -> ( Declaration, Linkage )
requestFn name op =
    let
        ( requestType, requestLinkage ) =
            Templates.L1.lowerType op.request

        ( responseType, responseLinkage ) =
            Templates.L1.lowerType op.response

        wrappedRespType =
            CG.fqTyped coreHttpMod "Request" [ CG.fqTyped coreDecodeMod "ResponseWrapper" [ responseType ] ]

        wrappedRespLinkage =
            CG.emptyLinkage
                |> CG.addImport (CG.importStmt coreHttpMod Nothing Nothing)
                |> CG.addImport (CG.importStmt coreDecodeMod Nothing Nothing)

        requestSig =
            CG.funAnn requestType wrappedRespType
    in
    ( CG.funDecl
        (Just "{-| AWS Endpoint. -}")
        (Just requestSig)
        name
        []
        CG.unit
    , CG.combineLinkage [ requestLinkage, responseLinkage, wrappedRespLinkage ]
    )



--== Types and Codecs


typeDeclarations : AWSApiModel -> ( List Declaration, List Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl ( declAccum, linkageAccum ) ->
            Templates.L1.typeDecl name decl
                |> Tuple.mapFirst (List.append declAccum)
                |> Tuple.mapSecond (List.append linkageAccum)
        )
        ( [], [] )
        model.declarations


jsonCodecs : AWSApiModel -> ( List Declaration, List Linkage )
jsonCodecs model =
    Dict.foldl
        (\name decl accum -> Templates.L1.codec name decl :: accum)
        []
        model.declarations
        |> List.unzip



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


coreHttpMod : List String
coreHttpMod =
    [ "AWS", "Core", "Http" ]


coreDecodeMod : List String
coreDecodeMod =
    [ "AWS", "Core", "Decode" ]
