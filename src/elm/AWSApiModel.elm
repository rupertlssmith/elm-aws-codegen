module AWSApiModel exposing (AWSApiModel, example)

import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Type(..))


type alias AWSApiModel =
    { name : List String
    , isRegional : Bool
    , endpointPrefix : String
    , apiVersion : String
    , protocol : String
    , signer : String
    , docs : String
    , declarations : Declarations
    , operations : Dict String ()
    }


example : AWSApiModel
example =
    { name = [ "Some", "Module" ]
    , isRegional = True
    , endpointPrefix = "dynamodb"
    , apiVersion = "2012-08-10"
    , protocol = "json"
    , signer = "signV4"
    , docs = ""
    , declarations =
        Dict.fromList
            [ ( "record", exampleRecord )
            , ( "custom", exampleCustom )
            ]
    , operations = Dict.empty
    }


exampleRecord : Declarable
exampleRecord =
    TProduct
        [ ( "a", TBasic BInt )
        , ( "b", TBasic BBool )
        , ( "c", TBasic BReal )
        , ( "d", TBasic BString )
        , ( "e", TBasic BString |> CList |> TContainer )
        , ( "f", TBasic BString |> CSet |> TContainer )
        , ( "g", CDict (TBasic BString) (TBasic BString) |> TContainer )
        , ( "h", TBasic BString |> COptional |> TContainer )
        ]
        |> DAlias


exampleCustom : Declarable
exampleCustom =
    DSum
        [ ( "a", [ ( "val", TBasic BInt ) ] )
        , ( "b", [ ( "val", TBasic BBool ) ] )
        , ( "c", [ ( "val", TBasic BReal ) ] )
        , ( "d", [ ( "val", TBasic BString ) ] )
        , ( "e", [ ( "val", TBasic BString |> CList |> TContainer ) ] )
        , ( "f", [ ( "val", TBasic BString |> CSet |> TContainer ) ] )
        , ( "g", [ ( "vals", CDict (TBasic BString) (TBasic BString) |> TContainer ) ] )
        , ( "h", [ ( "maybeVal", TBasic BString |> COptional |> TContainer ) ] )
        ]
