module Diff exposing (..)

import Json.Decode.Generic exposing (Json(..))


type alias Diff =
    ( List String, String )


diffsToString : List Diff -> String
diffsToString diffs =
    List.map
        (\( path, mismatch ) -> "")
        diffs
        |> List.intersperse "\n"
        |> String.concat


diff : Json -> Json -> List Diff
diff left right =
    diffInner left right []


diffInner : Json -> Json -> List String -> List ( List String, String )
diffInner left right path =
    case ( left, right ) of
        ( JString svall, JString svalr ) ->
            if svall == svalr then
                []

            else
                [ ( path, "string values" ) ]

        ( JBool bvall, JBool bvalr ) ->
            if bvall == bvalr then
                []

            else
                [ ( path, "bool values" ) ]

        ( JInt ivall, JInt ivalr ) ->
            []

        ( JFloat fvall, JFloat fvalr ) ->
            []

        ( JNull, JNull ) ->
            []

        ( JObj dictl, JObj dictr ) ->
            []

        ( JArr listl, JArr listr ) ->
            []

        ( _, _ ) ->
            []
