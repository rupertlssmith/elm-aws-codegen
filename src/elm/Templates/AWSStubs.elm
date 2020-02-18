module Templates.AWSStubs exposing (..)

import Dict
import Elm.CodeGen as CG exposing (File)
import L2 exposing (L2)
import L3 exposing (Processor)
import ResultME exposing (ResultME)


generator : Processor pos AWSStubsError
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
    Debug.todo "errorToString"


check : L2 pos -> ResultME err (L2 pos)
check =
    Debug.todo "check"


generate : L2 pos -> File
generate =
    Debug.todo "generate"
