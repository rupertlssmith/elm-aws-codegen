module HttpMethod exposing (HttpMethod(..), httpMethodCodec, httpMethodEnum)

import Codec exposing (Codec)
import Enum exposing (Enum)


type HttpMethod
    = DELETE
    | GET
    | HEAD
    | OPTIONS
    | POST
    | PUT


httpMethodEnum : Enum HttpMethod
httpMethodEnum =
    Enum.define
        [ DELETE
        , GET
        , HEAD
        , OPTIONS
        , POST
        , PUT
        ]
        (\val ->
            case val of
                DELETE ->
                    "DELETE"

                GET ->
                    "GET"

                HEAD ->
                    "HEAD"

                OPTIONS ->
                    "OPTIONS"

                POST ->
                    "POST"

                PUT ->
                    "PUT"
        )


httpMethodCodec : Codec HttpMethod
httpMethodCodec =
    Codec.build (Enum.encoder httpMethodEnum) (Enum.decoder httpMethodEnum)
