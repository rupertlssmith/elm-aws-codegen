module Templates.Api exposing (coreServiceMod, docs, globalService, module_, regionalService, service, serviceFile)

import AWSApiModel exposing (AWSApiModel)
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Declaration, File, Linkage, Module, TopLevelExpose)
import LevelOne exposing (Basic(..), Container(..), Declarable(..), Declarations, Type(..))
import Templates.L1 exposing (..)


serviceFile : AWSApiModel -> File
serviceFile model =
    let
        ( serviceFn, linkage ) =
            service model

        ( types, linkage2 ) =
            typeDeclarations model

        ( codecs, linkage3 ) =
            jsonCodecs model

        declarations =
            codecs
                |> List.append types
                |> (::) serviceFn

        linkages =
            linkage3
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
            CG.signature "service"
                (CG.funAnn
                    (CG.fqTyped coreServiceMod "Region" [])
                    (CG.fqTyped coreServiceMod "Service" [])
                )

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
            CG.signature "service"
                (CG.fqTyped coreServiceMod "Service" [])

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



--== Types and Codecs
-- {{~ it.types.filter(t => t.exposeAs) :t }}
-- {{= t.typeDef }}
--
-- {{? t.decoderDef }}
-- {{= t.decoderDef }}
-- {{?}}
--
-- {{? t.toStringDef }}
-- {{= t.toStringDef }}
-- {{?}}
-- {{~}}
--
-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}


typeDeclarations : AWSApiModel -> ( List Declaration, List Linkage )
typeDeclarations model =
    Dict.foldl
        (\name decl accum -> Templates.L1.typeDecl name decl :: accum)
        []
        model.declarations
        |> List.unzip


jsonCodecs : AWSApiModel -> ( List Declaration, List Linkage )
jsonCodecs model =
    Dict.foldl
        (\name decl accum -> Templates.L1.codec name decl :: accum)
        []
        model.declarations
        |> List.unzip



--== Operations
--
-- {{~ it.types.filter(t => t.exposeAs || t.category === 'request') :t }}
-- {{? it.metadata.protocol === 'json' && t.jsonEncoderDef }}
-- {{= t.jsonEncoderDef }}
-- {{?}}
--
-- {{? it.metadata.protocol === 'query' && t.queryEncoderDef }}
-- {{= t.queryEncoderDef }}
-- {{?}}
-- {{~}}
