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

        outlineDict =
            outline service.shapes

        mappings =
            modelShapes service.shapes outlineDict

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



--== First pass.
-- In the first pass all the named shapes are discovered and an approximate
-- outline of how they will transalate into L1 is generated. Either they are
-- basic types, or refer to things that will be given declared names.


type Outline
    = OlNamed String
    | OlBasic Basic


outline : Dict String Shape -> Dict String Outline
outline shapes =
    Dict.foldl
        (\key value accum ->
            case outlineShape value key of
                Nothing ->
                    accum

                Just ol ->
                    Dict.insert key ol accum
        )
        Dict.empty
        shapes


outlineShape : Shape -> String -> Maybe Outline
outlineShape shape name =
    case shape.type_ of
        AString ->
            outlineString shape name |> Just

        ABoolean ->
            BBool |> OlBasic |> Just

        AInteger ->
            outlineInt shape name |> Just

        ALong ->
            BInt |> OlBasic |> Just

        AFloat ->
            BReal |> OlBasic |> Just

        ADouble ->
            BReal |> OlBasic |> Just

        ABlob ->
            Nothing

        AStructure ->
            name |> OlNamed |> Just

        AList ->
            name |> OlNamed |> Just

        AMap ->
            name |> OlNamed |> Just

        ATimestamp ->
            name |> OlNamed |> Just

        AUnknown ->
            Nothing


outlineString : Shape -> String -> Outline
outlineString shape name =
    case
        ( shape.enum
        , Maybe.Extra.isJust shape.max
            || Maybe.Extra.isJust shape.min
            || Maybe.Extra.isJust shape.pattern
        )
    of
        ( Just enumVals, False ) ->
            OlNamed name

        ( Nothing, True ) ->
            OlNamed name

        ( _, _ ) ->
            OlBasic BString


outlineInt : Shape -> String -> Outline
outlineInt shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            OlNamed name

        _ ->
            OlBasic BInt



--== Second pass.
-- In the second pass a complete L1 model is generated for each shape. The
-- outline from the first pass is used to guide this, as all basic types are
-- inlined without being given intermediate names, and all types with declared
-- names will be used by referring to them.


modelShapes : Dict String Shape -> Dict String Outline -> Dict String (Result String Declarable)
modelShapes shapeDict outlineDict =
    Dict.map
        (\key value -> modelShape outlineDict value key)
        shapeDict


modelShape : Dict String Outline -> Shape -> String -> Result String Declarable
modelShape outlineDict shape name =
    case shape.type_ of
        AString ->
            modelString outlineDict shape name

        ABoolean ->
            BBool |> TBasic |> DAlias |> Ok

        AInteger ->
            modelInt outlineDict shape name

        ALong ->
            BInt |> TBasic |> DAlias |> Ok

        AFloat ->
            BReal |> TBasic |> DAlias |> Ok

        ADouble ->
            BReal |> TBasic |> DAlias |> Ok

        ABlob ->
            Err "Blob not implemented."

        AStructure ->
            modelStructure outlineDict shape name

        AList ->
            modelList outlineDict shape name

        AMap ->
            CDict (BString |> TBasic) (BString |> TBasic) |> TContainer |> DAlias |> Ok

        ATimestamp ->
            BString |> TBasic |> DAlias |> Ok

        AUnknown ->
            Err "Unknown not implemented."


modelString : Dict String Outline -> Shape -> String -> Result String Declarable
modelString outlineDict shape name =
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


modelInt : Dict String Outline -> Shape -> String -> Result String Declarable
modelInt outlineDict shape name =
    case Maybe.Extra.isJust shape.max || Maybe.Extra.isJust shape.min of
        True ->
            RInt { min = shape.min, max = shape.max, width = Nothing }
                |> DRestricted
                |> Ok

        _ ->
            BInt |> TBasic |> DAlias |> Ok


modelStructure : Dict String Outline -> Shape -> String -> Result String Declarable
modelStructure outlineDict shape name =
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


modelList : Dict String Outline -> Shape -> String -> Result String Declarable
modelList outlineDict shape name =
    CList (BString |> TBasic) |> TContainer |> DAlias |> Ok
