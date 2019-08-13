module Service exposing (Service, serviceCodec)

{-| AWS Service2 Descriptor. This module provides the data model and decoders.
-}

import Codec exposing (Codec)
import Dict exposing (Dict)


type alias Service =
    { version : Maybe String
    , metaData : MetaData
    , operations : Dict String Operation
    , shapes : Dict String Shape
    , documentation : Maybe String
    }


serviceCodec =
    Codec.object Service
        |> Codec.optionalField "version" .version Codec.string
        |> Codec.field "metadata" .metaData metaDataCodec
        |> Codec.field "operations" .operations (Codec.dict operationCodec)
        |> Codec.field "shapes" .shapes (Codec.dict shapeCodec)
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type alias MetaData =
    { apiVersion : String
    , endpointPrefix : String
    , jsonVersion : Maybe String
    , protocol : String
    , serviceAbbreviation : Maybe String
    , serviceFullName : Maybe String
    , serviceId : String
    , signatureVersion : Maybe String
    , targetPrefix : Maybe String
    , uid : Maybe String
    }


metaDataCodec =
    Codec.object MetaData
        |> Codec.field "apiVersion" .apiVersion Codec.string
        |> Codec.field "endpointPrefix" .endpointPrefix Codec.string
        |> Codec.optionalField "jsonVersion" .jsonVersion Codec.string
        |> Codec.field "protocol" .protocol Codec.string
        |> Codec.optionalField "serviceAbbreviation" .serviceAbbreviation Codec.string
        |> Codec.optionalField "serviceFullName" .serviceFullName Codec.string
        |> Codec.field "serviceId" .serviceId Codec.string
        |> Codec.optionalField "signatureVersion" .signatureVersion Codec.string
        |> Codec.optionalField "targetPrefix" .targetPrefix Codec.string
        |> Codec.optionalField "uid" .uid Codec.string
        |> Codec.buildObject


type alias Operation =
    { name : String
    , http : Http
    , input : Maybe ShapeRef
    , output : Maybe ShapeRef
    , errors : Maybe (List ShapeRef)
    , idempotent : Maybe Bool
    , documentation : Maybe String
    }


operationCodec =
    Codec.object Operation
        |> Codec.field "name" .name Codec.string
        |> Codec.field "http" .http httpCodec
        |> Codec.optionalField "input" .input shapeRefCodec
        |> Codec.optionalField "output" .output shapeRefCodec
        |> Codec.optionalField "errors" .errors (Codec.list shapeRefCodec)
        |> Codec.optionalField "idempotent" .idempotent Codec.bool
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type alias Http =
    { method : String
    , requestUri : Maybe String
    }


httpCodec =
    Codec.object Http
        |> Codec.field "method" .method Codec.string
        |> Codec.optionalField "requestUri" .requestUri Codec.string
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
    , member : Maybe ShapeRef
    , key : Maybe ShapeRef
    , value : Maybe ShapeRef
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
        |> Codec.optionalField "member" .member shapeRefCodec
        |> Codec.optionalField "key" .key shapeRefCodec
        |> Codec.optionalField "value" .value shapeRefCodec
        |> Codec.optionalField "enum" .enum (Codec.list Codec.string)
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.buildObject


type Types
    = String -- Enum
    | Boolean
    | Integer
    | Long
    | Float
    | Double
    | Blob
    | Structure
    | List
    | Map
    | Timestamp
    | Unknown


typesCodec : Codec Types
typesCodec =
    Codec.map
        (\val ->
            case val of
                "string" ->
                    String

                "boolean" ->
                    Boolean

                "integer" ->
                    Integer

                "long" ->
                    Long

                "float" ->
                    Float

                "double" ->
                    Double

                "blob" ->
                    Blob

                "structure" ->
                    Structure

                "list" ->
                    List

                "map" ->
                    Map

                "timestamp" ->
                    Timestamp

                str ->
                    let
                        _ =
                            Debug.log "unknown" str
                    in
                    Unknown
        )
        (\types ->
            case types of
                String ->
                    "string"

                Boolean ->
                    "boolean"

                Integer ->
                    "integer"

                Long ->
                    "long"

                Float ->
                    "float"

                Double ->
                    "double"

                Blob ->
                    "blob"

                Structure ->
                    "structure"

                List ->
                    "list"

                Map ->
                    "map"

                Timestamp ->
                    "timestamp"

                Unknown ->
                    "unknown"
        )
        Codec.string
