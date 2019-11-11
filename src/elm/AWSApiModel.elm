module AWSApiModel exposing (AWSApiModel, Endpoint)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Type(..))


type alias Endpoint =
    { request : Type
    , response : Type
    , requestTypeName : String
    , responseTypeName : String
    , url : String
    , httpMethod : String
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
    , declarations : Declarations
    , operations : Dict String Endpoint
    }
