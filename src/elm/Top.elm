port module Top exposing (main)

import AWSApiModel
import AWSService exposing (AWSService)
import Codec
import Dict exposing (Dict)
import Diff
import Elm.Pretty
import Elm.Writer
import Json.Decode as Decode
import Json.Decode.Generic as Generic
import Pretty
import Random exposing (Seed)
import Task
import Templates.Api
import Time exposing (Posix)
import Transform



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : ( String, String ) -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Error _ ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort (\( name, value ) -> ModelData name value)
                ]



-- State Machine


type Model
    = Initial
    | Seeded { seed : Seed }
    | LoadedModel { seed : Seed, dataModel : AWSService }
    | ModelProcessed
    | TemplateProcessed
    | Done
    | Error String



-- Events


type Msg
    = CreateSeed Posix
    | ModelData String String


init : a -> ( Model, Cmd Msg )
init _ =
    ( Initial
    , Task.perform CreateSeed Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initial, CreateSeed posix ) ->
            ( Seeded { seed = Random.initialSeed <| Time.posixToMillis posix }, Cmd.none )

        ( Seeded { seed }, ModelData name val ) ->
            processServiceModel name val seed

        ( _, _ ) ->
            ( model, Cmd.none )


processServiceModel : String -> String -> Seed -> ( Model, Cmd Msg )
processServiceModel name val seed =
    let
        serviceResult =
            Codec.decodeString AWSService.awsServiceCodec val
    in
    case serviceResult of
        Ok service ->
            let
                _ =
                    Debug.log "=== Processing ===" service.metaData.serviceId

                ( codegen, errors ) =
                    Transform.transform service
                        |> Tuple.mapFirst Templates.Api.serviceFile
                        |> Tuple.mapFirst Elm.Pretty.pretty
                        |> Tuple.mapFirst (Pretty.pretty 120)
            in
            ( Seeded { seed = seed }
            , ( service.metaData.serviceId ++ ".elm", codegen ) |> codeOutPort
            )

        Err err ->
            let
                _ =
                    Debug.log "Error" (name ++ " - " ++ Decode.errorToString err)
            in
            ( Seeded { seed = seed }, Cmd.none )
