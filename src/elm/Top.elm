port module Top exposing (main)

import AWSService exposing (AWSService)
import Codec
import Dict exposing (Dict)
import Diff
import Elm.Writer
import Json.Decode as Decode
import Json.Decode.Generic as Generic
import Random exposing (Seed)
import Task
import Templates.Api
import Time exposing (Posix)



-- Top level construction


main : Program () Model Msg
main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }



-- Ports for data input and output


port modelInPort : (( String, String ) -> msg) -> Sub msg


port codeOutPort : String -> Cmd msg


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


init _ =
    ( Initial
    , Task.perform CreateSeed Time.now
    )


update msg model =
    case ( model, msg ) of
        ( Initial, CreateSeed posix ) ->
            ( Seeded { seed = Random.initialSeed <| Time.posixToMillis posix }, Cmd.none )

        ( Seeded { seed }, ModelData name val ) ->
            processServiceModel name val seed

        ( _, _ ) ->
            ( model, Cmd.none )


processServiceModel name val seed =
    let
        example =
            Codec.decodeString AWSService.awsServiceCodec val
    in
    case example of
        Ok service ->
            let
                _ =
                    Debug.log "Ok" name

                original =
                    Decode.decodeString Generic.json val

                parsed =
                    Decode.decodeString Generic.json (Codec.encodeToString 0 AWSService.awsServiceCodec service)

                diffs =
                    case ( original, parsed ) of
                        ( Ok jsonl, Ok jsonr ) ->
                            Diff.diff jsonl jsonr |> Diff.diffsToString |> logIfVal "Diffs"

                        ( _, _ ) ->
                            "Failed to generic decode" |> Debug.log "Error"

                codegen =
                    Templates.Api.serviceFile Templates.Api.example
                        |> Elm.Writer.writeFile
                        |> Elm.Writer.write
                        |> Debug.log "codegen"
            in
            ( Seeded { seed = seed }
            , codegen |> codeOutPort
            )

        Err err ->
            let
                _ =
                    Debug.log "Error" (name ++ " - " ++ Decode.errorToString err)
            in
            ( Seeded { seed = seed }
            , Decode.errorToString err |> codeOutPort
            )


logIfVal : String -> String -> String
logIfVal label val =
    if val == "" then
        val

    else
        Debug.log label val
