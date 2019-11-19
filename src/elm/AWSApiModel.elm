module AWSApiModel exposing (AWSApiModel, Endpoint)

import Dict exposing (Dict)
import HttpMethod exposing (HttpMethod)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Outlined, Type(..))


type alias Endpoint =
    { httpMethod : HttpMethod
    , url : String
    , request : Maybe ( String, Type Outlined )
    , response : Maybe ( String, Type Outlined )
    }


type alias AWSApiModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : String
    , signer : String
    , docs : String
    , xmlNamespace : Maybe String
    , targetPrefix : Maybe String
    , signingName : Maybe String
    , jsonVersion : Maybe String
    , declarations : Declarations Outlined
    , operations : Dict String Endpoint
    }
