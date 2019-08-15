module Templates.Api exposing (..)

import Elm.Syntax.Module
import Elm.Syntax.ModuleName


type alias Module =
    { name : String
    , docs : String
    , imports : List ()
    , operations : List ()
    , types_ : List ()
    }



-- module AWS.{{= it.mod }}
--     exposing
--         ( service
--         , {{= it.operationNames.join('\n        , ')}}
--         , {{= it.types.filter(t => t.exposeAs).map(t => t.exposeAs).join('\n        , ')}}
--         )
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
-- {{= it.serviceDefinition }}
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
