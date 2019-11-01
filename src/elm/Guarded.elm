module Guarded exposing (..)


type Guarded i a e
    = Guarded (i -> Result e a) (e -> String) (a -> i)


make : (i -> Result e a) -> (e -> String) -> (a -> i) -> Guarded
make =
    Guarded


build : Guarded i a e -> i -> Result e a


decoder : Guarded i a e -> Decoder a


encoder : Guarded i a e -> a -> Value



-- Helper guard functions for numbers.


type IntError
    = BelowRange
    | AboveRange


gt : Int -> Int -> Result IntError Int


lt : Int -> Int -> Result IntError Int



-- Helper guard functions for strings.


type StringError
    = TooShort
    | TooLong
    | NotMatchingRegex


minLength : Int -> String -> Result StringError String


maxLength : Int -> String -> Result StringError String


regexMatch : String -> String -> Result StringError String
