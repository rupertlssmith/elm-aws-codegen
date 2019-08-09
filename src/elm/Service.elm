module Service exposing (Service)

{-| AWS Service2 Descriptor. This module provides the data model and decoders.
-}

import Codec
import Dict exposing (Dict)


type alias Service =
    { version : String
    , metaData : MetaData
    , operations : Operations
    , shapes : Shapes
    , documentation : Maybe String
    }


serviceCodec =
    Codec.object Service
        |> Codec.field "version" .version Codec.string
        |> Codec.field "metadata" .metaData metaDataCodec
        |> Codec.field "operations" .operations operationsCodec
        |> Codec.field "shapes" .shapes shapesCodec
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type alias MetaData =
    { apiVersion : String
    , endpointPrefix : String
    , jsonVersion : String
    , protocol : String
    , serviceAbbreviation : String
    , serviceFullName : String
    , serviceId : String
    , signatureVersion : String
    , targetPrefix : String
    , uid : String
    }


type alias Operations =
    Dict String Operation


type alias Operation =
    { name : String
    , http : Http
    , input : ShapeRef
    , errors : List ShapeRef
    , documentation : String
    }


type alias Http =
    { method : String
    , requestUri : String
    }


type alias ShapeRef =
    { shape : String
    , documentation : Maybe String
    }


type alias Shapes =
    Dict String Shape


type alias Shape =
    { type_ : Types
    , required : List String
    , max : Maybe Int
    , min : Maybe Int
    , pattern : Maybe String
    , members : List ShapeRef
    , enum : List String
    , documentation : Maybe String
    }


type Types
    = Integer
    | String -- Enum
    | Blob
    | List
    | Structure
