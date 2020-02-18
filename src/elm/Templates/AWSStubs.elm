module Templates.AWSStubs exposing (AWSStubsError(..), check, defaultProperties, errorToString, generate, generator)

import Dict
import Elm.CodeGen as CG exposing (File)
import L1 exposing (Properties, Property(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, Processor)
import ResultME exposing (ResultME)


defaultProperties : DefaultProperties
defaultProperties =
    { top =
        Dict.fromList
            [ ( "name", PQName [ "AWS" ] "Stub" )
            , ( "isRegional", PBool False )
            , ( "apiVersion", PString "1.0" )
            , ( "protocol", PEnum "JSON" )
            , ( "signer", PEnum "V4" )
            , ( "xmlNamespace", POptional Nothing )
            , ( "targetPrefix", POptional Nothing )
            , ( "signingName", POptional Nothing )
            , ( "jsonVersion", POptional Nothing )
            , ( "documentation", POptional Nothing )
            ]
    , alias =
        Dict.fromList
            [ ( "awsStubGeneration", POptional Nothing ) ]
    , sum = Dict.empty
    , enum =
        Dict.fromList
            [ ( "elmEnumStyle", PEnum "customType" ) ]
    , fields = Dict.empty
    }


generator : Processor pos AWSStubsError
generator =
    { name = "AWSStubs"
    , defaults = defaultProperties
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
