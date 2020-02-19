module Templates.AWSStubs exposing (AWSStubsError(..), check, defaultProperties, errorToString, generate, generator)

import Dict
import Elm.CodeGen as CG exposing (File)
import Enum exposing (Enum)
import L1 exposing (PropSpec(..), Properties, Property(..))
import L2 exposing (L2)
import L3 exposing (DefaultProperties, L3, Processor)
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


awsStubGenerationEnum : Enum String
awsStubGenerationEnum =
    Enum.define [ "model", "endpoint" ] identity


defaultProperties : DefaultProperties
defaultProperties =
    { top =
        L1.defineProperties
            [ ( "name", PSQName )
            , ( "xmlNamespace", PSOptional PSString )
            , ( "targetPrefix", PSOptional PSString )
            , ( "signingName", PSOptional PSString )
            , ( "jsonVersion", PSOptional PSString )
            , ( "documentation", PSOptional PSString )
            ]
            [ ( "isRegional", PBool False )
            , ( "apiVersion", PString "1.0" )
            , ( "protocol", PEnum protocolEnum "JSON" )
            , ( "signer", PEnum signerEnum "V4" )
            ]
    , alias =
        L1.defineProperties
            [ ( "awsStubGeneration", PSOptional (PSEnum awsStubGenerationEnum) ) ]
            []
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


check : L3 pos -> ResultME err (L3 pos)
check =
    Debug.todo "check"


generate : L3 pos -> File
generate =
    Debug.todo "generate"
