module Templates.AWSStubs exposing (AWSStubsError(..), check, defaultProperties, errorToString, generate, generator)

import Dict
import Elm.CodeGen as CG exposing (File)
import Enum exposing (Enum)
import L1 exposing (PropSpec(..), Properties, Property(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, Processor)
import ResultME exposing (ResultME)


protocolEnum : Enum String
protocolEnum =
    Enum.define [ "JSON" ] identity


signerEnum : Enum String
signerEnum =
    Enum.define [ "V4" ] identity


elmEnumStyleEnum : Enum String
elmEnumStyleEnum =
    Enum.define [ "customType", "guardedType", "listOfStrings" ] identity


defaultProperties : DefaultProperties
defaultProperties =
    { top =
        L1.defineProperties
            []
            [ ( "name", PQName [ "AWS" ] "Stub" )
            , ( "isRegional", PBool False )
            , ( "apiVersion", PString "1.0" )
            , ( "protocol", PEnum protocolEnum "JSON" )
            , ( "signer", PEnum signerEnum "V4" )
            , ( "xmlNamespace", POptional PSString Nothing )
            , ( "targetPrefix", POptional PSString Nothing )
            , ( "signingName", POptional PSString Nothing )
            , ( "jsonVersion", POptional PSString Nothing )
            , ( "documentation", POptional PSString Nothing )
            ]
    , alias =
        L1.defineProperties
            []
            [ ( "awsStubGeneration", POptional PSString Nothing ) ]
    , sum = L1.defineProperties [] []
    , enum =
        L1.defineProperties
            []
            [ ( "elmEnumStyle", PEnum elmEnumStyleEnum "customType" ) ]
    , fields = L1.defineProperties [] []
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
