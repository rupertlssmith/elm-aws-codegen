module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel)
import AWSService exposing (AWSService)


transform : AWSService -> AWSApiModel
transform _ =
    AWSApiModel.example
