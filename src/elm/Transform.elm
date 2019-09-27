module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel)
import AWSService exposing (AWSService, Shape)
import Dict exposing (Dict)
import LevelOne exposing (Basic(..), Container(..), Declarable(..), Declarations, Type(..))


transform : AWSService -> AWSApiModel
transform service =
    let
        default =
            AWSApiModel.example
    in
    { default | operations = [] }


modelShapes : Dict String Shape -> Declarations
modelShapes shapeDict =
    Dict.empty
