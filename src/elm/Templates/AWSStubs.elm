module AWSStubs exposing (..)

import L3 exposing (L3)


generator : L3 pos AWSStubsError
generator =
    { name = "AWSStubs"
    , defaults = Dict.empty
    , check = check
    , errorToString = errorToString
    }


type AWSStubsError
    = AWSStubsError


errorToString : (pos -> String) -> pos -> err -> String
errorToString =
    ()


check : L2 pos -> ResultME err (L2 pos)
check =
    ()


generate : L2 pos -> File
generate =
    ()
