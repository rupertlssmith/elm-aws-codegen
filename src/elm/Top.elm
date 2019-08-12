port module Top exposing (main)

import Codec
import Dict exposing (Dict)
import Random exposing (Seed)
import Service exposing (Service)
import Task
import Time exposing (Posix)



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (String -> msg) -> Sub msg


port codeOutPort : String -> Cmd msg


subscriptions model =
    case model of
        Error _ ->
            Sub.none

        _ ->
            Sub.batch
                [ modelInPort ModelData
                ]



-- State Machine


type Model
    = Initial
    | Seeded { seed : Seed }
    | LoadedModel { seed : Seed, dataModel : Service }
    | ModelProcessed
    | TemplateProcessed
    | Done
    | Error String



-- Events


type Msg
    = CreateSeed Posix
    | ModelData String


init _ =
    ( Initial
    , Task.perform CreateSeed Time.now
    )


update msg model =
    case ( model, msg ) of
        ( Initial, CreateSeed posix ) ->
            ( Seeded { seed = Random.initialSeed <| Time.posixToMillis posix }, Cmd.none )

        ( Seeded { seed }, ModelData val ) ->
            let
                example =
                    Codec.decodeString Service.serviceCodec val
            in
            case example of
                Ok service ->
                    ( LoadedModel { seed = seed, dataModel = service }
                    , Codec.encodeToString 4 Service.serviceCodec service |> codeOutPort
                    )

                Err _ ->
                    ( Error "Could not decode.", Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )
