module Transform exposing (transform)

import AWSApiModel exposing (AWSApiModel)
import AWSService exposing (AWSService, AWSType(..), Shape)
import Dict exposing (Dict)
import L1 exposing (Basic(..), Container(..), Declarable(..), Declarations, Restricted(..), Type(..))
import Maybe.Extra


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
            modelString shapeDict shape name

        ABoolean ->
            BBool |> TBasic |> DAlias |> Ok

        AInteger ->
            modelInt shapeDict shape name

        ALong ->
            BInt |> TBasic |> DAlias |> Ok

        AFloat ->
            BReal |> TBasic |> DAlias |> Ok

        ADouble ->
            BReal |> TBasic |> DAlias |> Ok

        ABlob ->
            Err "Blob not implemented."

        AStructure ->
            modelStructure shapeDict shape name

        AList ->
            CList (BString |> TBasic) |> TContainer |> DAlias |> Ok

        AMap ->
            CDict (BString |> TBasic) (BString |> TBasic) |> TContainer |> DAlias |> Ok

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            Err "Unknown not implemented."


modelString : Dict String Shape -> Shape -> String -> Result String Declarable
modelString shapeDict shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            List.map
                (\val -> ( val, [ ( "name", BString |> TBasic ) ] ))
                enumVals
                |> DSum
                |> Ok

        ( Nothing, True ) ->
            RString { minLength = shape.min, maxLength = shape.max, regex = shape.pattern }
                |> DRestricted
                |> Ok

        ( _, _ ) ->
            BString |> TBasic |> DAlias |> Ok


modelInt : Dict String Shape -> Shape -> String -> Result String Declarable
modelInt shapeDict shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            RInt { min = shape.min, max = shape.max, width = Nothing }
                |> DRestricted
                |> Ok

        _ ->
            BInt |> TBasic |> DAlias |> Ok


modelStructure : Dict String Shape -> Shape -> String -> Result String Declarable
modelStructure shapeDict shape name =
    case shape.members of
        Nothing ->
            Err (name ++ ": structure has no members")

        Just members ->
            Dict.foldl
                (\key value accum -> ( key, BString |> TBasic ) :: accum)
                []
                members
                |> TProduct
                |> DAlias
                |> Ok
