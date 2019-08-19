module Templates.Api exposing (..)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import ElmDSL


type alias GenModel =
    { name : List String
    , docs : String
    , imports : List ()
    , operations : List ()
    , types_ : List ()
    }


example : GenModel
example =
    { name = [ "Some", "Module" ]
    , docs = ""
    , imports = []
    , operations = []
    , types_ = []
    }


file : GenModel -> File
file model =
    ElmDSL.file (module_ model) (imports model) [ service model ] []



-- module AWS.{{= it.mod }}
--     exposing
--         ( service
--         , {{= it.operationNames.join('\n        , ')}}
--         , {{= it.types.filter(t => t.exposeAs).map(t => t.exposeAs).join('\n        , ')}}
--         )


module_ : GenModel -> Module
module_ model =
    ElmDSL.normalModule model.name []



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
    [ ElmDSL.import_ [ "AWS", "Core", "Decode" ] Nothing Nothing
    , ElmDSL.import_ [ "AWS", "Core", "Encode" ] Nothing Nothing
    , ElmDSL.import_ [ "AWS", "Core", "Http" ] Nothing Nothing
    , ElmDSL.import_ [ "AWS", "Core", "Service" ] Nothing Nothing
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
    let
        signature =
            ElmDSL.signature "service" ElmDSL.unit
    in
    ElmDSL.functionDeclaration
        Nothing
        Nothing
        "service"
        [ ElmDSL.varPattern "x" ]
        (ElmDSL.functionOrValue [] "x")



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
