module Service exposing (Service, serviceCodec)

{-| AWS Service2 Descriptor. This module provides the data model and decoders.
-}

import Codec exposing (Codec)
import Dict exposing (Dict)


type alias Service =
    { version : String
    , metaData : MetaData
    , operations : Dict String Operation
    , shapes : Dict String Shape
    , documentation : Maybe String
    }


serviceCodec =
    Codec.object Service
        |> Codec.field "version" .version Codec.string
        |> Codec.field "metadata" .metaData metaDataCodec
        |> Codec.field "operations" .operations (Codec.dict operationCodec)
        |> Codec.field "shapes" .shapes (Codec.dict shapeCodec)
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


metaDataCodec =
    Codec.object MetaData
        |> Codec.field "apiVersion" .apiVersion Codec.string
        |> Codec.field "endpointPrefix" .endpointPrefix Codec.string
        |> Codec.field "jsonVersion" .jsonVersion Codec.string
        |> Codec.field "protocol" .protocol Codec.string
        |> Codec.field "serviceAbbreviation" .serviceAbbreviation Codec.string
        |> Codec.field "serviceFullName" .serviceFullName Codec.string
        |> Codec.field "serviceId" .serviceId Codec.string
        |> Codec.field "signatureVersion" .signatureVersion Codec.string
        |> Codec.field "targetPrefix" .targetPrefix Codec.string
        |> Codec.field "uid" .uid Codec.string
        |> Codec.buildObject


type alias Operation =
    { name : String
    , http : Http
    , input : ShapeRef
    , errors : List ShapeRef
    , documentation : String
    }


operationCodec =
    Codec.object Operation
        |> Codec.field "name" .name Codec.string
        |> Codec.field "http" .http httpCodec
        |> Codec.field "input" .input shapeRefCodec
        |> Codec.field "errors" .errors (Codec.list shapeRefCodec)
        |> Codec.field "documentation" .documentation Codec.string
        |> Codec.buildObject


type alias Http =
    { method : String
    , requestUri : String
    }


httpCodec =
    Codec.object Http
        |> Codec.field "method" .method Codec.string
        |> Codec.field "requestUri" .requestUri Codec.string
        |> Codec.buildObject


type alias ShapeRef =
    { shape : String
    , documentation : Maybe String
    }


shapeRefCodec =
    Codec.object ShapeRef
        |> Codec.field "shape" .shape Codec.string
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type alias Shape =
    { type_ : Types
    , required : Maybe (List String)
    , max : Maybe Int
    , min : Maybe Int
    , pattern : Maybe String
    , members : Maybe (Dict String ShapeRef)
    , enum : Maybe (List String)
    , documentation : Maybe String
    }


shapeCodec =
    Codec.object Shape
        |> Codec.field "type" .type_ typesCodec
        |> Codec.optionalField "required" .required (Codec.list Codec.string)
        |> Codec.optionalField "max" .max Codec.int
        |> Codec.optionalField "min" .min Codec.int
        |> Codec.optionalField "pattern" .pattern Codec.string
        |> Codec.optionalField "members" .members (Codec.dict shapeRefCodec)
        |> Codec.optionalField "enum" .enum (Codec.list Codec.string)
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type Types
    = Integer
    | String -- Enum
    | Blob
    | List
    | Structure
    | Unknown


typesCodec : Codec Types
typesCodec =
    Codec.map
        (\val ->
            case val of
                "integer" ->
                    Integer

                "string" ->
                    String

                "blob" ->
                    Blob

                "list" ->
                    List

                "structure" ->
                    Structure

                _ ->
                    Unknown
        )
        (\types ->
            case types of
                Integer ->
                    "integer"

                String ->
                    "string"

                Blob ->
                    "blob"

                List ->
                    "list"

                Structure ->
                    "structure"

                Unknown ->
                    "unknown"
        )
        Codec.string
