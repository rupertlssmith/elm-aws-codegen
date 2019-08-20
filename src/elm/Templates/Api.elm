module Templates.Api exposing (GenModel, coreServiceMod, docs, example, globalService, module_, regionalService, service, serviceFile)

import ElmDSL exposing (..)
import Templates.L1 exposing (..)


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
    , endpointPrefix = "dynamodb"
    , apiVersion = "2012-08-10"
    , protocol = "json"
    , signer = "signV4"
    , docs = ""
    , imports = []
    , operations = []
    , types_ = []
    }


serviceFile : GenModel -> File
serviceFile model =
    let
        ( functions, fullImportsAndExposing ) =
            List.unzip
                [ service model ]

        ( deDupedImports, deDupedExposing ) =
            deDupeImportsAndExposing fullImportsAndExposing

        moduleSpec =
            module_ model deDupedExposing
    in
    file moduleSpec deDupedImports functions []


coreServiceMod : List String
coreServiceMod =
    [ "AWS", "Core", "Service" ]



--== Module Specification (with exposing).


module_ : GenModel -> List TopLevelExpose -> Module
module_ model exposings =
    normalModule model.name exposings



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


service : GenModel -> ( Declaration, ImportsAndExposing )
service model =
    if model.isRegional then
        regionalService model

    else
        globalService model


regionalService : GenModel -> ( Declaration, ImportsAndExposing )
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
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyImportsAndExposing
        |> addImport (import_ coreServiceMod Nothing Nothing)
        |> addExposing (functionExpose "service")
    )


globalService : GenModel -> ( Declaration, ImportsAndExposing )
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
    ( functionDeclaration
        (Just "{-| Configuration for this service. -}")
        (Just sig)
        "service"
        []
        impl
    , emptyImportsAndExposing
        |> addImport (import_ coreServiceMod Nothing Nothing)
        |> addExposing (functionExpose "service")
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


t =
    ()



-- {-| {{= it.doc }}
-- -}
-- type {{= it.type }}
--     = {{= it.type }}_{{= it.enum.join(`\n    | ${it.type}_`) }}


unionType =
    ()



-- {-| {{= it.doc }}
-- -}
-- type alias {{= it.type }} =
--     { {{= it.members.map(it.memberType).join('\n    , ') }}
--     }


recordType =
    ()



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
