module Templates.L1 exposing (typeDecl, codec)

{-| Elm code generation from LevelOne models.

@docs typeDecl, codec

-}

import Codec
import ElmDSL exposing (..)
import LevelOne exposing (..)
import String.Case as Case



--== Type Declarations


{-| Turns an L1 type declaration into Elm code.
-}
typeDecl : String -> Declarable -> ( Declaration, ImportsAndExposing )
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
typeAlias : String -> Type -> ( Declaration, ImportsAndExposing )
typeAlias name l1Type =
    ( aliasDeclaration Nothing (Case.toCamelCaseUpper name) [] (lowerType l1Type)
    , emptyImportsAndExposing
    )


{-| Turns an L1 sum type into a custom type in Elm code.
-}
customType : String -> List ( String, Type ) -> ( Declaration, ImportsAndExposing )
customType name constructors =
    let
        mappedConstructors =
            List.map
                (Tuple.mapBoth Case.toCamelCaseUpper (\l1Type -> [ lowerType l1Type ]))
                constructors
    in
    ( customTypeDeclaration Nothing (Case.toCamelCaseUpper name) [] mappedConstructors
    , emptyImportsAndExposing
    )


{-| Lowers an L1 type into an Elm type annotation.
-}
lowerType : Type -> TypeAnnotation
lowerType l1Type =
    case l1Type of
        TBasic basic ->
            lowerBasic basic

        TNamed name ->
            genericType (Case.toCamelCaseUpper name)

        TProduct fields ->
            lowerProduct fields

        TContainer container ->
            lowerContainer container

        TFunction arg res ->
            unit


{-| Lowers an L1 basic type into an Elm type annotation.
-}
lowerBasic : Basic -> TypeAnnotation
lowerBasic basic =
    case basic of
        BBool ->
            genericType "Bool"

        BInt ->
            genericType "Int"

        BReal ->
            genericType "Float"

        BString ->
            genericType "String"


{-| Lowers an L1 product type into an Elm type annotation.
-}
lowerProduct : List ( String, Type ) -> TypeAnnotation
lowerProduct fields =
    let
        mappedFields =
            List.map
                (Tuple.mapBoth Case.toCamelCaseUpper lowerType)
                fields
    in
    record mappedFields


{-| Lowers an L1 container type into an Elm type annotation.
-}
lowerContainer : Container -> TypeAnnotation
lowerContainer container =
    case container of
        CList l1Type ->
            typed [] "List" [ lowerType l1Type ]

        CSet l1Type ->
            typed [] "Set" [ lowerType l1Type ]

        CDict l1keyType l1valType ->
            typed [] "Dict" [ lowerType l1keyType, lowerType l1valType ]

        COptional l1Type ->
            typed [] "Maybe" [ lowerType l1Type ]



--== Decoders and Encoders


{-| Generates a Codec for an L1 type declaration.
-}
codec : String -> Declarable -> ( Declaration, ImportsAndExposing )
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
typeAliasCodec : String -> Type -> ( Declaration, ImportsAndExposing )
typeAliasCodec name l1Type =
    let
        codecFnName =
            Case.toCamelCaseLower (name ++ "Codec")

        typeName =
            Case.toCamelCaseUpper name

        sig =
            signature codecFnName
                (typed [] "Codec" [ typed [] typeName [] ])

        impl =
            codecNamedType name l1Type
    in
    ( functionDeclaration
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , emptyImportsAndExposing
        |> addImport (import_ codecMod Nothing (Just <| explicit [ typeOrAliasExpose "Codec" ]))
    )


{-| Generates a Codec for an L1 sum type.
-}
customTypeCodec : String -> List ( String, Type ) -> ( Declaration, ImportsAndExposing )
customTypeCodec name constructors =
    -- let
    --     mappedConstructors =
    --         List.map
    --             (Tuple.mapBoth Case.toCamelCaseUpper (\l1Type -> [ lowerType l1Type ]))
    --             constructors
    -- in
    -- ( customTypeDeclaration Nothing (Case.toCamelCaseUpper name) [] mappedConstructors
    -- , emptyImportsAndExposing
    --     |> addImport (import_ codecMod Nothing (Just <| explicit [ typeOrAliasExpose "Codec" ]))
    -- )
    dummy name


{-| Generates a Codec for an L1 type that has been named as an alias.
-}
codecNamedType : String -> Type -> Expression
codecNamedType name l1Type =
    case l1Type of
        TBasic basic ->
            codecType l1Type

        TNamed named ->
            -- genericType (Case.toCamelCaseUpper name)
            unitExpr

        TProduct fields ->
            codecNamedProduct name fields

        TContainer container ->
            codecType l1Type

        TFunction arg res ->
            unitExpr


{-| Generates a Codec for an L1 type.
-}
codecType : Type -> Expression
codecType l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic

        TNamed named ->
            -- genericType (Case.toCamelCaseUpper name)
            unitExpr

        TProduct fields ->
            codecProduct fields

        _ ->
            unitExpr


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> Type -> Expression
codecTypeField name l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic
                |> codecField name

        TNamed named ->
            -- genericType (Case.toCamelCaseUpper name)
            unitExpr

        TProduct fields ->
            codecProduct fields
                |> codecField name

        TContainer container ->
            codecContainerField name container

        TFunction arg res ->
            unitExpr


{-| Generates a codec for a basic L1 type.
-}
codecBasic : Basic -> Expression
codecBasic basic =
    case basic of
        BBool ->
            functionOrValue [ "Codec" ] "bool"

        BInt ->
            functionOrValue [ "Codec" ] "int"

        BReal ->
            functionOrValue [ "Codec" ] "float"

        BString ->
            functionOrValue [ "Codec" ] "string"


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
                    (application
                        [ functionOrValue [ "Codec" ] "object"
                        , functionOrValue [] typeName
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
    let
        mappedFields =
            List.map
                (Tuple.mapBoth Case.toCamelCaseUpper codecType)
                fields
    in
    unitExpr


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Codec.optionalField`.
-}
codecContainerField : String -> Container -> Expression
codecContainerField name container =
    case container of
        CList l1Type ->
            application [ functionOrValue [ "Codec" ] "list", codecType l1Type ]
                |> codecField name

        CSet l1Type ->
            application [ functionOrValue [ "Codec" ] "set", codecType l1Type ]
                |> codecField name

        CDict l1keyType l1valType ->
            application [ functionOrValue [ "Codec" ] "dict", codecType l1keyType, codecType l1valType ]
                |> codecField name

        COptional l1Type ->
            codecType l1Type
                |> codecOptionalField name


{-| Outputs codecs for a list of fields and terminates the list with `Codec.buildObject`.
Helper function useful when building record codecs.
-}
codecFields fields =
    List.foldr (\( fieldName, l1Type ) accum -> codecTypeField fieldName l1Type :: accum)
        [ application
            [ functionOrValue [ "Codec" ] "buildObject"
            ]
        ]
        fields


{-| Helper function for building field codecs.
-}
codecField : String -> Expression -> Expression
codecField name expr =
    application
        [ functionOrValue [] "Codec.field"
        , literal (Case.toCamelCaseLower name)
        , recordAccessFunction (Case.toCamelCaseLower name)
        , expr
        ]


{-| Helper function for building optional field codecs.
-}
codecOptionalField : String -> Expression -> Expression
codecOptionalField name expr =
    application
        [ functionOrValue [] "Codec.optionalField"
        , literal (Case.toCamelCaseLower name)
        , recordAccessFunction (Case.toCamelCaseLower name)
        , expr
        ]



--== Helper Functions


dummy : String -> ( Declaration, ImportsAndExposing )
dummy name =
    ( functionDeclaration Nothing Nothing name [] unitExpr, emptyImportsAndExposing )


codecMod : List String
codecMod =
    [ "Codec" ]
