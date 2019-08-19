module Templates.Api exposing (..)

import ElmDSL
    exposing
        ( Declaration
        , File
        , Import
        , Module
        , application
        , file
        , functionDeclaration
        , functionOrValue
        , functionTypeAnnotation
        , import_
        , literal
        , normalModule
        , signature
        , typed
        , unit
        , varPattern
        )


type alias GenModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : String
    , signer : String
    , docs : String
    , imports : List ()
    , operations : List ()
    , types_ : List ()
    }


example : GenModel
example =
    { name = [ "Some", "Module" ]
    , isRegional = True
    , endpointPrefix = ""
    , apiVersion = "2012-08-10"
    , protocol = "json"
    , signer = "v4"
    , docs = ""
    , imports = []
    , operations = []
    , types_ = []
    }


serviceFile : GenModel -> File
serviceFile model =
    file (module_ model) (imports model) [ service model ] []


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



-- module AWS.{{= it.mod }}
--     exposing
--         ( service
--         , {{= it.operationNames.join('\n        , ')}}
--         , {{= it.types.filter(t => t.exposeAs).map(t => t.exposeAs).join('\n        , ')}}
--         )


module_ : GenModel -> Module
module_ model =
    normalModule model.name []



--
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
--
-- import AWS.Core.Decode
-- import AWS.Core.Encode
-- import AWS.Core.Http
-- import AWS.Core.Service
-- import Json.Decode as JD
-- import Json.Decode.Pipeline as JDP
-- {{? it.metadata.protocol === 'json' }}import Json.Encode as JE
-- {{?}}
-- {{~ it.extraImports :importExtra }}{{= importExtra }}
-- {{~}}
--


imports : GenModel -> List Import
imports model =
    [ import_ [ "AWS", "Core", "Decode" ] Nothing Nothing
    , import_ [ "AWS", "Core", "Encode" ] Nothing Nothing
    , import_ [ "AWS", "Core", "Http" ] Nothing Nothing
    , import_ coreServiceMod Nothing Nothing
    ]



-- {{= it.serviceDefinition }}
-- service : {{? it.isRegional }}AWS.Core.Service.Region -> {{?}}AWS.Core.Service.Service
-- service ={{? it.isRegional }}
--     AWS.Core.Service.defineRegional{{?? true }}
--     AWS.Core.Service.defineGlobal{{?}}
--         "{{= it.endpointPrefix }}"
--         "{{= it.apiVersion }}"
--         AWS.Core.Service.{{= it.protocol }}
--         AWS.Core.Service.{{= it.signer }}
--         {{= it.extra }}


service : GenModel -> Declaration
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


regionalService : GenModel -> Declaration
regionalService model =
    let
        sig =
            signature "service"
                (functionTypeAnnotation
                    (typed coreServiceMod "Region" [])
                    (typed coreServiceMod "Service" [])
                )

        impl =
            application
                [ functionOrValue coreServiceMod "defineRegional"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    functionDeclaration
        Nothing
        (Just sig)
        "service"
        []
        impl


globalService : GenModel -> Declaration
globalService model =
    let
        sig =
            signature "service"
                (typed coreServiceMod "Service" [])

        impl =
            application
                [ functionOrValue coreServiceMod "defineGlobal"
                , literal model.endpointPrefix
                , literal model.apiVersion
                , functionOrValue coreServiceMod model.protocol
                , functionOrValue coreServiceMod model.signer
                ]
    in
    functionDeclaration
        Nothing
        (Just sig)
        "service"
        []
        impl



--
--
-- -- OPERATIONS
--
-- {{= it.operations.join('\n\n') }}
--
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
--
-- {{~ it.types.filter(t => t.category === 'request') :t }}
-- {{= t.typeDef }}
-- {{~}}
--
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
