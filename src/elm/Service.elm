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
    , signingName : Maybe String
    , globalEndpoint : Maybe String
    , xmlNamespace : Maybe String
    , checksumFormat : Maybe String
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
        |> Codec.optionalField "signingName" .signingName Codec.string
        |> Codec.optionalField "globalEndpoint" .globalEndpoint Codec.string
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.optionalField "checksumFormat" .checksumFormat Codec.string
        |> Codec.buildObject


type alias Operation =
    { name : String
    , http : Http
    , input : Maybe ShapeRef
    , output : Maybe ShapeRef
    , errors : Maybe (List ShapeRef)
    , idempotent : Maybe Bool
    , endpoint : Maybe Endpoint
    , documentation : Maybe String
    , deprecated : Maybe Bool
    , deprecatedMessage : Maybe String
    , documentationUrl : Maybe String
    , alias : Maybe String
    , authtype : Maybe String
    }


operationCodec =
    Codec.object Operation
        |> Codec.field "name" .name Codec.string
        |> Codec.field "http" .http httpCodec
        |> Codec.optionalField "input" .input shapeRefCodec
        |> Codec.optionalField "output" .output shapeRefCodec
        |> Codec.optionalField "errors" .errors (Codec.list shapeRefCodec)
        |> Codec.optionalField "idempotent" .idempotent Codec.bool
        |> Codec.optionalField "endpoint" .endpoint endpointCodec
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "deprecatedMessage" .deprecatedMessage Codec.string
        |> Codec.optionalField "documentationUrl" .documentationUrl Codec.string
        |> Codec.optionalField "alias" .alias Codec.string
        |> Codec.optionalField "authtype" .authtype Codec.string
        |> Codec.buildObject


type alias Endpoint =
    { hostPrefix : String
    }


endpointCodec =
    Codec.object Endpoint
        |> Codec.field "hostPrefix" .hostPrefix Codec.string
        |> Codec.buildObject


type alias Http =
    { method : String
    , requestUri : Maybe String
    , responseCode : Maybe Int
    , requireUri : Maybe Bool
    }


httpCodec =
    Codec.object Http
        |> Codec.field "method" .method Codec.string
        |> Codec.optionalField "requestUri" .requestUri Codec.string
        |> Codec.optionalField "responseCode" .responseCode Codec.int
        |> Codec.optionalField "requireUri" .requireUri Codec.bool
        |> Codec.buildObject


type alias Error =
    { code : String
    , httpStatusCode : Int
    , senderFault : Bool
    }


errorCodec =
    Codec.object Error
        |> Codec.field "code" .code Codec.string
        |> Codec.field "httpStatusCode" .httpStatusCode Codec.int
        |> Codec.field "senderFault" .senderFault Codec.bool
        |> Codec.buildObject


type alias ShapeRef =
    { shape : String
    , documentation : Maybe String
    , idempotencyToken : Maybe String
    , location : Maybe String
    , locationName : Maybe String
    , deprecated : Maybe Bool
    , box : Maybe Bool
    , resultWrapper : Maybe String
    , flattened : Maybe Bool
    , deprecatedMessage : Maybe String
    , xmlNamespace : Maybe String
    , xmlAttribute : Maybe String
    , streaming : Maybe Bool
    , eventpayload : Maybe Bool
    , jsonvalue : Maybe Bool
    , queryName : Maybe String
    , timestampFormat : Maybe String
    }


shapeRefCodec =
    Codec.object ShapeRef
        |> Codec.field "shape" .shape Codec.string
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "idempotencyToken" .idempotencyToken Codec.string
        |> Codec.optionalField "location" .location Codec.string
        |> Codec.optionalField "locationName" .locationName Codec.string
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "box" .box Codec.bool
        |> Codec.optionalField "resultWrapper" .resultWrapper Codec.string
        |> Codec.optionalField "flattened" .flattened Codec.bool
        |> Codec.optionalField "deprecatedMessage" .deprecatedMessage Codec.string
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.optionalField "xmlAttribute" .xmlAttribute Codec.string
        |> Codec.optionalField "streaming" .streaming Codec.bool
        |> Codec.optionalField "eventpayload" .eventpayload Codec.bool
        |> Codec.optionalField "jsonvalue" .jsonvalue Codec.bool
        |> Codec.optionalField "queryName" .queryName Codec.string
        |> Codec.optionalField "timestampFormat" .timestampFormat Codec.string
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
    , sensitive : Maybe Bool
    , deprecated : Maybe Bool
    , box : Maybe Bool
    , exception : Maybe Bool
    , fault : Maybe Bool
    , flattened : Maybe Bool
    , documentation : Maybe String
    , locationName : Maybe String
    , timestampFormat : Maybe String
    , payload : Maybe String
    , wrapper : Maybe Bool
    , event : Maybe Bool
    , xmlNamespace : Maybe String
    , error : Maybe Error
    , eventstream : Maybe Bool
    , streaming : Maybe Bool
    , xmlOrder : Maybe (List String)
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
        |> Codec.optionalField "sensitive" .sensitive Codec.bool
        |> Codec.optionalField "deprecated" .deprecated Codec.bool
        |> Codec.optionalField "box" .box Codec.bool
        |> Codec.optionalField "exception" .exception Codec.bool
        |> Codec.optionalField "fault" .fault Codec.bool
        |> Codec.optionalField "flattened" .flattened Codec.bool
        |> Codec.optionalField "documentation" .documentation Codec.string
        |> Codec.optionalField "locationName" .locationName Codec.string
        |> Codec.optionalField "timestampFormat" .timestampFormat Codec.string
        |> Codec.optionalField "payload" .payload Codec.string
        |> Codec.optionalField "wrapper" .wrapper Codec.bool
        |> Codec.optionalField "event" .event Codec.bool
        |> Codec.optionalField "xmlNamespace" .xmlNamespace Codec.string
        |> Codec.optionalField "error" .error errorCodec
        |> Codec.optionalField "eventstream" .eventstream Codec.bool
        |> Codec.optionalField "streaming" .streaming Codec.bool
        |> Codec.optionalField "xmlOrder" .xmlOrder (Codec.list Codec.string)
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
