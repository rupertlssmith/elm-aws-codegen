module Diff exposing (..)

import Dict
import Json.Decode.Generic exposing (Json(..))


type alias Diff =
    ( List String, String )


diffsToString : List Diff -> String
diffsToString diffs =
    List.map
        (\( path, mismatch ) -> "At " ++ String.join " -> " path ++ " " ++ mismatch)
        diffs
        |> List.intersperse "\n"
        |> String.concat


diff : Json -> Json -> List Diff
diff left right =
    diffInner left right [ "root" ]


diffInner : Json -> Json -> List String -> List ( List String, String )
diffInner left right path =
    case ( left, right ) of
        ( JString svall, JString svalr ) ->
            if svall == svalr then
                []

            else
                [ ( List.reverse path, ": string values" ) ]

        ( JBool bvall, JBool bvalr ) ->
            if bvall == bvalr then
                []

            else
                [ ( List.reverse path, ": bool values" ) ]

        ( JInt ivall, JInt ivalr ) ->
            if ivall == ivalr then
                []

            else
                [ ( List.reverse path, ": int values" ) ]

        ( JFloat fvall, JFloat fvalr ) ->
            if fvall == fvalr then
                []

            else
                [ ( List.reverse path, ": float values" ) ]

        ( JNull, JNull ) ->
            []

        ( JObj dictl, JObj dictr ) ->
            let
                ddiff =
                    Dict.diff dictl dictr

                fieldDiffs =
                    List.foldl
                        (\key accum -> ( List.reverse path, ": key " ++ key ) :: accum)
                        []
                        (Dict.keys ddiff)

                matchingFields =
                    Dict.intersect dictl dictr
                        |> Dict.keys

                childDiffs =
                    List.foldl
                        (\key accum ->
                            case ( Dict.get key dictl, Dict.get key dictr ) of
                                ( Just fieldl, Just fieldr ) ->
                                    diffInner fieldl fieldr (key :: path)
                                        |> List.append accum

                                ( _, _ ) ->
                                    accum
                        )
                        []
                        matchingFields
            in
            List.append fieldDiffs childDiffs

        ( JArr arrl, JArr arrr ) ->
            []

        ( _, _ ) ->
            [ ( List.reverse path, ": json types" ) ]
