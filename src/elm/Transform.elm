module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel)
import AWSService exposing (AWSService, AWSType(..), Shape)
import Dict exposing (Dict)
import LevelOne exposing (Basic(..), Container(..), Declarable(..), Declarations, Type(..))


transform : AWSService -> AWSApiModel
transform service =
    let
        default =
            AWSApiModel.example

        mappings =
            modelShapes service.shapes

        ( okMappings, errMappings ) =
            Dict.foldl
                (\key val ( okAccum, errAccum ) ->
                    case val of
                        Ok decl ->
                            ( Dict.insert key decl okAccum, errAccum )

                        Err err ->
                            ( okAccum, Dict.insert key err errAccum )
                )
                ( Dict.empty, Dict.empty )
                mappings

        _ =
            Debug.log "errors" errMappings
    in
    { default | declarations = okMappings }


modelShapes : Dict String Shape -> Dict String (Result String Declarable)
modelShapes shapeDict =
    Dict.map
        (\key value -> modelShape shapeDict value key)
        shapeDict


modelShape : Dict String Shape -> Shape -> String -> Result String Declarable
modelShape shapeDict shape name =
    case shape.type_ of
        AString ->
            BString |> TBasic |> DAlias |> Ok

        ABoolean ->
            BBool |> TBasic |> DAlias |> Ok

        AInteger ->
            BInt |> TBasic |> DAlias |> Ok

        ALong ->
            BInt |> TBasic |> DAlias |> Ok

        AFloat ->
            BReal |> TBasic |> DAlias |> Ok

        ADouble ->
            BReal |> TBasic |> DAlias |> Ok

        ABlob ->
            Err "Blob not implemented."

        AStructure ->
            --TProduct [] |> DAlias |> Ok
            Err "Structure not implemented."

        AList ->
            CList (BString |> TBasic) |> TContainer |> DAlias |> Ok

        AMap ->
            CDict (BString |> TBasic) (BString |> TBasic) |> TContainer |> DAlias |> Ok

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            Err "Unknown not implemented."
