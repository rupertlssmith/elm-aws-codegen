module Templates.L1 exposing (typeDecl, codec)

{-| Elm code generation from LevelOne models.

@docs typeDecl, codec

-}

import Codec
import Elm.CodeGen exposing (..)
import LevelOne exposing (..)
import String.Case as Case



--== Type Declarations


{-| Turns an L1 type declaration into Elm code.
-}
typeDecl : String -> Declarable -> ( Declaration, Linkage )
typeDecl name decl =
    case decl of
        DAlias l1Type ->
            typeAlias name l1Type

        DSum constructors ->
            customType name constructors

        DRestricted _ ->
            dummy name


{-| Turns an L1 `Type` into a type alias in Elm code.
-}
typeAlias : String -> Type -> ( Declaration, Linkage )
typeAlias name l1Type =
    let
        ( loweredType, linkage ) =
            lowerType l1Type
    in
    ( aliasDecl Nothing (Case.toCamelCaseUpper name) [] loweredType
    , linkage
    )


{-| Turns an L1 sum type into a custom type in Elm code.
-}
customType : String -> List ( String, List ( String, Type ) ) -> ( Declaration, Linkage )
customType name constructors =
    let
        lowerArgs ( _, l1Type ) =
            lowerType l1Type

        ( mappedConstructors, linkages ) =
            List.map
                (\( consName, consArgs ) ->
                    let
                        ( loweredArgs, linkage ) =
                            List.map lowerArgs consArgs
                                |> List.unzip
                                |> Tuple.mapSecond combineLinkage
                    in
                    ( ( Case.toCamelCaseLower consName, loweredArgs ), linkage )
                )
                constructors
                |> List.unzip
    in
    ( customTypeDecl Nothing (Case.toCamelCaseUpper name) [] mappedConstructors
    , combineLinkage linkages
    )


{-| Lowers an L1 type into an Elm type annotation.
-}
lowerType : Type -> ( TypeAnnotation, Linkage )
lowerType l1Type =
    case l1Type of
        TBasic basic ->
            ( lowerBasic basic
            , emptyLinkage
            )

        TNamed name ->
            ( typed (Case.toCamelCaseUpper name) []
            , emptyLinkage
            )

        TProduct fields ->
            lowerProduct fields

        TContainer container ->
            lowerContainer container

        TFunction arg res ->
            ( unitAnn
            , emptyLinkage
            )


{-| Lowers an L1 basic type into an Elm type annotation.
-}
lowerBasic : Basic -> TypeAnnotation
lowerBasic basic =
    case basic of
        BBool ->
            boolAnn

        BInt ->
            intAnn

        BReal ->
            floatAnn

        BString ->
            stringAnn


{-| Lowers an L1 product type into an Elm type annotation.
-}
lowerProduct : List ( String, Type ) -> ( TypeAnnotation, Linkage )
lowerProduct fields =
    let
        ( mappedFields, linkages ) =
            List.map
                (\( name, l1Type ) ->
                    let
                        ( loweredType, linkage ) =
                            lowerType l1Type
                    in
                    ( ( Case.toCamelCaseLower name, loweredType ), linkage )
                )
                fields
                |> List.unzip
    in
    ( recordAnn mappedFields
    , combineLinkage linkages
    )


{-| Lowers an L1 container type into an Elm type annotation.
-}
lowerContainer : Container -> ( TypeAnnotation, Linkage )
lowerContainer container =
    case container of
        CList l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst listAnn

        CSet l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst setAnn
                |> Tuple.mapSecond (addImport setImport)

        CDict l1keyType l1valType ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( dictAnn keyAnn valAnn
            , combineLinkage [ keyLink, valLink ] |> addImport dictImport
            )

        COptional l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst maybeAnn



--== Decoders and Encoders


{-| Generates a Codec for an L1 type declaration.
-}
codec : String -> Declarable -> ( Declaration, Linkage )
codec name decl =
    case decl of
        DAlias l1Type ->
            typeAliasCodec name l1Type

        DSum constructors ->
            customTypeCodec name constructors

        DRestricted _ ->
            dummy (name ++ "Codec")


{-| Generates a Codec for an L1 type alias.
-}
typeAliasCodec : String -> Type -> ( Declaration, Linkage )
typeAliasCodec name l1Type =
    let
        codecFnName =
            Case.toCamelCaseLower (name ++ "Codec")

        typeName =
            Case.toCamelCaseUpper name

        sig =
            signature codecFnName
                (typed "Codec" [ typed typeName [] ])

        impl =
            codecNamedType name l1Type
    in
    ( funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , emptyLinkage
        |> addImport codecImport
    )


{-| Generates a Codec for an L1 sum type.
-}
customTypeCodec : String -> List ( String, List ( String, Type ) ) -> ( Declaration, Linkage )
customTypeCodec name constructors =
    let
        codecFnName =
            Case.toCamelCaseLower (name ++ "Codec")

        typeName =
            Case.toCamelCaseUpper name

        sig =
            signature codecFnName
                (typed "Codec" [ typed typeName [] ])

        impl =
            codecCustomType constructors
    in
    ( funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , emptyLinkage
        |> addImport codecImport
    )


codecCustomType : List ( String, List ( String, Type ) ) -> Expression
codecCustomType constructors =
    let
        codecVariant name args =
            List.foldr
                (\( _, l1Type ) accum -> codecType l1Type :: accum)
                [ Case.toCamelCaseUpper name |> fun
                , Case.toCamelCaseUpper name |> string
                , codecFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> apply
    in
    List.foldr (\( name, consArgs ) accum -> codecVariant name consArgs :: accum)
        [ apply [ codecFn "buildCustom" ] ]
        constructors
        |> pipe
            (apply
                [ codecFn "custom"
                , codecMatchFn constructors
                ]
            )


codecMatchFn : List ( String, List ( String, Type ) ) -> Expression
codecMatchFn constructors =
    let
        consFnName name =
            "f" ++ Case.toCamelCaseLower name

        args =
            List.foldr (\( name, _ ) accum -> (consFnName name |> varPattern) :: accum)
                [ varPattern "value" ]
                constructors

        consPattern ( name, consArgs ) =
            ( namedPattern (Case.toCamelCaseUpper name)
                (List.map (\( argName, _ ) -> varPattern argName) consArgs)
            , List.foldr (\( argName, _ ) accum -> val argName :: accum)
                [ consFnName name |> fun ]
                consArgs
                |> List.reverse
                |> apply
            )

        matchFnBody =
            List.map consPattern constructors
                |> caseExpr (val "value")
    in
    lambda args matchFnBody


{-| Generates a Codec for an L1 type that has been named as an alias.
-}
codecNamedType : String -> Type -> Expression
codecNamedType name l1Type =
    case l1Type of
        TBasic basic ->
            codecType l1Type

        TNamed named ->
            unit

        TProduct fields ->
            codecNamedProduct name fields

        TContainer container ->
            codecType l1Type

        TFunction arg res ->
            unit


{-| Generates a Codec for an L1 type.
-}
codecType : Type -> Expression
codecType l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic

        TNamed named ->
            unit

        TProduct fields ->
            codecProduct fields

        TContainer container ->
            codecContainer container

        _ ->
            unit


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> Type -> Expression
codecTypeField name l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic
                |> codecField name

        TNamed named ->
            unit

        TProduct fields ->
            codecProduct fields
                |> codecField name

        TContainer container ->
            codecContainerField name container

        TFunction arg res ->
            unit


{-| Generates a codec for a basic L1 type.
-}
codecBasic : Basic -> Expression
codecBasic basic =
    case basic of
        BBool ->
            codecFn "bool"

        BInt ->
            codecFn "int"

        BReal ->
            codecFn "float"

        BString ->
            codecFn "string"


codecContainer : Container -> Expression
codecContainer container =
    case container of
        CList l1Type ->
            apply [ codecFn "list", codecType l1Type ]
                |> parens

        CSet l1Type ->
            apply [ codecFn "set", codecType l1Type ]
                |> parens

        CDict _ l1valType ->
            apply [ codecFn "dict", codecType l1valType ]
                |> parens

        COptional l1Type ->
            apply [ codecFn "maybe", codecType l1Type ]
                |> parens


{-| Generates a codec for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
codecNamedProduct : String -> List ( String, Type ) -> Expression
codecNamedProduct name fields =
    let
        typeName =
            Case.toCamelCaseUpper name

        impl =
            codecFields fields
                |> pipe
                    (apply
                        [ codecFn "object"
                        , fun typeName
                        ]
                    )
    in
    impl


{-| Generates a codec for an L1 product type that does not have a name.
Without a name there is no constructor function for the product, so it must be
built explicitly by its fields.
-}
codecProduct : List ( String, Type ) -> Expression
codecProduct fields =
    unit


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Codec.optionalField`.
-}
codecContainerField : String -> Container -> Expression
codecContainerField name container =
    case container of
        CList l1Type ->
            apply [ codecFn "list", codecType l1Type ]
                |> parens
                |> codecField name

        CSet l1Type ->
            apply [ codecFn "set", codecType l1Type ]
                |> parens
                |> codecField name

        CDict _ l1valType ->
            apply [ codecFn "dict", codecType l1valType ]
                |> parens
                |> codecField name

        COptional l1Type ->
            codecType l1Type
                |> codecOptionalField name



--== Helper Functions


{-| Outputs codecs for a list of fields and terminates the list with `Codec.buildObject`.
Helper function useful when building record codecs.
-}
codecFields fields =
    List.foldr (\( fieldName, l1Type ) accum -> codecTypeField fieldName l1Type :: accum)
        [ apply
            [ codecFn "buildObject"
            ]
        ]
        fields


{-| Helper function for building field codecs.
-}
codecField : String -> Expression -> Expression
codecField name expr =
    apply
        [ codecFn "field"
        , string (Case.toCamelCaseLower name)
        , accessFun (Case.toCamelCaseLower name)
        , expr
        ]


{-| Helper function for building optional field codecs.
-}
codecOptionalField : String -> Expression -> Expression
codecOptionalField name expr =
    apply
        [ codecFn "optionalField"
        , string (Case.toCamelCaseLower name)
        , accessFun (Case.toCamelCaseLower name)
        , expr
        ]


dummy : String -> ( Declaration, Linkage )
dummy name =
    ( funDecl Nothing Nothing name [] unit, emptyLinkage )


codecMod : List String
codecMod =
    [ "Codec" ]


codecFn : String -> Expression
codecFn =
    fqFun codecMod


codecImport : Import
codecImport =
    importStmt codecMod Nothing (Just <| exposeExplicit [ typeOrAliasExpose "Codec" ])


setImport : Import
setImport =
    importStmt [ "Set" ] Nothing (Just <| exposeExplicit [ typeOrAliasExpose "Set" ])


dictImport : Import
dictImport =
    importStmt [ "Dict" ] Nothing (Just <| exposeExplicit [ typeOrAliasExpose "Dict" ])
