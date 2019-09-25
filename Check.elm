module Check exposing (..)

import Codec exposing (Codec)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Record =
    { a : Int
    , b : Bool
    , c : Float
    , d : String
    , e : List String
    , f : Set String
    , g : Dict String String
    , h : Maybe String
    }


type Custom
    = A Int
    | B Bool
    | C Float
    | D String
    | E (List String)
    | F (Set String)
    | G (Dict String String)
    | H (Maybe String)


{-| Codec for Record.
-}
recordCodec : Codec Record
recordCodec =
    Codec.object Record
        |> Codec.field "a" .a Codec.int
        |> Codec.field "b" .b Codec.bool
        |> Codec.field "c" .c Codec.float
        |> Codec.field "d" .d Codec.string
        |> Codec.field "e" .e (Codec.list Codec.string)
        |> Codec.field "f" .f (Codec.set Codec.string)
        |> Codec.field "g" .g (Codec.dict Codec.string)
        |> Codec.optionalField "h" .h Codec.string
        |> Codec.buildObject


{-| Codec for Custom.
-}
customCodec : Codec Custom
customCodec =
    Codec.custom
        (\fa fb fc fd fe ff fg fh value ->
            case value of
                A val ->
                    fa val

                B val ->
                    fb val

                C val ->
                    fc val

                D val ->
                    fd val

                E val ->
                    fe val

                F val ->
                    ff val

                G vals ->
                    fg vals

                H maybeVal ->
                    fh maybeVal
        )
        |> Codec.variant1 "A" A Codec.int
        |> Codec.variant1 "B" B Codec.bool
        |> Codec.variant1 "C" C Codec.float
        |> Codec.variant1 "D" D Codec.string
        |> Codec.variant1 "E" E (Codec.list Codec.string)
        |> Codec.variant1 "F" F (Codec.set Codec.string)
        |> Codec.variant1 "G" G (Codec.dict Codec.string)
        |> Codec.variant1 "H" H (Codec.maybe Codec.string)
        |> Codec.buildCustom
