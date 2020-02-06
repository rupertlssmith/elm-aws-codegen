module Errors exposing (Error, append, combine, map, single, toList)

{-| Utility for building up multiple errors during some process that can report
one or more error cases.

TODO: Get rid of this, as its really just a nonempty list, for which libs already
exist.

-}


type Error a
    = SingleError a
    | MultipleError (List (Error a))


single : a -> Error a
single val =
    SingleError val


append : a -> Error a -> Error a
append val errs =
    case errs of
        SingleError err ->
            MultipleError [ SingleError val, SingleError err ]

        MultipleError errorList ->
            MultipleError (SingleError val :: errorList)


combine : List (Error a) -> Error a
combine errs =
    MultipleError errs


map : (a -> b) -> Error a -> Error b
map mapFn errs =
    case errs of
        SingleError err ->
            SingleError (mapFn err)

        MultipleError errorList ->
            List.map (map mapFn) errorList
                |> MultipleError


toList : Error a -> List a
toList errs =
    case errs of
        SingleError err ->
            [ err ]

        MultipleError errorList ->
            List.map toList errorList
                |> List.concat
