module AWSApiModel exposing (AWSApiModel, Endpoint)

import AWS.Core.Service exposing (Protocol, Signer)
import Dict exposing (Dict)
import Elm.CodeGen as CG exposing (Comment, DocComment, FileComment)
import HttpMethod exposing (HttpMethod)
import L1 exposing (Basic(..), Container(..), Declarable(..), Type(..))
import L2 exposing (L2, RefChecked)


type alias Endpoint =
    { httpMethod : HttpMethod
    , url : String
    , request : Type RefChecked
    , response : Type RefChecked
    , documentation : Maybe (Comment DocComment)
    }


type alias AWSApiModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : Protocol
    , signer : Signer
    , xmlNamespace : Maybe String
    , targetPrefix : Maybe String
    , signingName : Maybe String
    , jsonVersion : Maybe String
    , declarations : L2
    , operations : Dict String Endpoint
    , documentation : Maybe (Comment FileComment)
    }
