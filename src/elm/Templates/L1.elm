module Templates.L1 exposing (typeDecl, codec)

{-| Elm code generation from L1 models.

@docs typeDecl, codec

-}

import Codec
import Elm.CodeGen as CG exposing (Declaration, Expression, Import, Linkage, TypeAnnotation)
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import String.Case as Case



--== Type Declarations


{-| Turns an L1 type declaration into Elm code.

A type can result in a list of declarations - enums in addition to declaring a
type can also declare the permitted enum values.

-}
typeDecl : String -> Declarable -> ( List Declaration, List Linkage )
typeDecl name decl =
    case decl of
        DAlias l1Type ->
            typeAlias name l1Type
                |> Tuple.mapBoth List.singleton List.singleton

        DSum constructors ->
            customType name constructors
                |> Tuple.mapBoth List.singleton List.singleton

        DEnum labels ->
            enumType name labels

        DRestricted res ->
            restrictedType name res
                |> Tuple.mapBoth List.singleton List.singleton


restrictedType : String -> Restricted -> ( Declaration, Linkage )
restrictedType name restricted =
    case restricted of
        RInt _ ->
            ( CG.customTypeDecl Nothing (Case.toCamelCaseUpper name) [] [ ( Case.toCamelCaseUpper name, [ CG.intAnn ] ) ]
            , CG.emptyLinkage
            )

        RString _ ->
            ( CG.customTypeDecl Nothing (Case.toCamelCaseUpper name) [] [ ( Case.toCamelCaseUpper name, [ CG.stringAnn ] ) ]
            , CG.emptyLinkage
            )


{-| Turns an L1 `Type` into a type alias in Elm code.
-}
typeAlias : String -> Type -> ( Declaration, Linkage )
typeAlias name l1Type =
    let
        ( loweredType, linkage ) =
            lowerType l1Type
    in
    ( CG.aliasDecl Nothing (Case.toCamelCaseUpper name) [] loweredType
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
                                |> Tuple.mapSecond CG.combineLinkage
                    in
                    ( ( Case.toCamelCaseUpper consName, loweredArgs ), linkage )
                )
                constructors
                |> List.unzip
    in
    ( CG.customTypeDecl Nothing (Case.toCamelCaseUpper name) [] mappedConstructors
    , CG.combineLinkage linkages
    )


{-| Turns an L1 enum type into a guarded type in Elm code.

This produces 2 declarations, one for the guarded type, and one for the enum
declaring its allowed values.

-}
enumType : String -> List String -> ( List Declaration, List Linkage )
enumType name labels =
    let
        guardedConstructor =
            [ ( Case.toCamelCaseUpper name, [ CG.stringAnn ] ) ]

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "make"
                , List.map
                    (\label ->
                        CG.apply
                            [ CG.fun (Case.toCamelCaseUpper name)
                            , Case.toCamelCaseUpper label |> CG.string
                            ]
                    )
                    labels
                    |> CG.list
                , CG.lambda [ CG.namedPattern (Case.toCamelCaseUpper name) [ CG.varPattern "val" ] ]
                    (CG.val "val")
                ]
    in
    ( [ CG.customTypeDecl Nothing (Case.toCamelCaseUpper name) [] guardedConstructor
      , CG.patternDecl (CG.varPattern (Case.toCamelCaseLower name)) enumValues
      ]
    , [ CG.emptyLinkage |> CG.addImport enumImport ]
    )


{-| Lowers an L1 type into an Elm type annotation.
-}
lowerType : Type -> ( TypeAnnotation, Linkage )
lowerType l1Type =
    case l1Type of
        TBasic basic ->
            ( lowerBasic basic
            , CG.emptyLinkage
            )

        TNamed name ->
            ( CG.typed (Case.toCamelCaseUpper name) []
            , CG.emptyLinkage
            )

        TProduct fields ->
            lowerProduct fields

        TContainer container ->
            lowerContainer container

        TFunction arg res ->
            ( CG.unitAnn
            , CG.emptyLinkage
            )


{-| Lowers an L1 basic type into an Elm type annotation.
-}
lowerBasic : Basic -> TypeAnnotation
lowerBasic basic =
    case basic of
        BBool ->
            CG.boolAnn

        BInt ->
            CG.intAnn

        BReal ->
            CG.floatAnn

        BString ->
            CG.stringAnn


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
    ( CG.recordAnn mappedFields
    , CG.combineLinkage linkages
    )


{-| Lowers an L1 container type into an Elm type annotation.
-}
lowerContainer : Container -> ( TypeAnnotation, Linkage )
lowerContainer container =
    case container of
        CList l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.listAnn

        CSet l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.setAnn
                |> Tuple.mapSecond (CG.addImport setImport)

        CDict l1keyType l1valType ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.dictAnn keyAnn valAnn
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictImport
            )

        COptional l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.maybeAnn



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

        DEnum labels ->
            enumCodec name labels

        DRestricted _ ->
            dummyFn (name ++ "Codec")


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
            CG.signature codecFnName
                (CG.typed "Codec" [ CG.typed typeName [] ])

        impl =
            codecNamedType name l1Type
    in
    ( CG.funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
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
            CG.signature codecFnName
                (CG.typed "Codec" [ CG.typed typeName [] ])

        impl =
            codecCustomType constructors
    in
    ( CG.funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
    )


enumCodec : String -> List String -> ( Declaration, Linkage )
enumCodec name constructors =
    dummyFn "enumCodec"


codecCustomType : List ( String, List ( String, Type ) ) -> Expression
codecCustomType constructors =
    let
        codecVariant name args =
            List.foldr
                (\( _, l1Type ) accum -> codecType l1Type :: accum)
                [ Case.toCamelCaseUpper name |> CG.fun
                , Case.toCamelCaseUpper name |> CG.string
                , codecFn ("variant" ++ String.fromInt (List.length args))
                ]
                args
                |> List.reverse
                |> CG.apply
    in
    List.foldr (\( name, consArgs ) accum -> codecVariant name consArgs :: accum)
        [ CG.apply [ codecFn "buildCustom" ] ]
        constructors
        |> CG.pipe
            (CG.apply
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
            List.foldr (\( name, _ ) accum -> (consFnName name |> CG.varPattern) :: accum)
                [ CG.varPattern "value" ]
                constructors

        consPattern ( name, consArgs ) =
            ( CG.namedPattern (Case.toCamelCaseUpper name)
                (List.map (\( argName, _ ) -> CG.varPattern argName) consArgs)
            , List.foldr (\( argName, _ ) accum -> CG.val argName :: accum)
                [ consFnName name |> CG.fun ]
                consArgs
                |> List.reverse
                |> CG.apply
            )

        matchFnBody =
            List.map consPattern constructors
                |> CG.caseExpr (CG.val "value")
    in
    CG.lambda args matchFnBody


{-| Generates a Codec for an L1 type that has been named as an alias.
-}
codecNamedType : String -> Type -> Expression
codecNamedType name l1Type =
    case l1Type of
        TBasic basic ->
            codecType l1Type

        TNamed named ->
            CG.string "codecNamedType_TNamed"

        TProduct fields ->
            codecNamedProduct name fields

        TContainer container ->
            codecType l1Type

        TFunction arg res ->
            CG.unit


{-| Generates a Codec for an L1 type.
-}
codecType : Type -> Expression
codecType l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic

        TNamed named ->
            CG.string "codecType_TNamed"

        TProduct fields ->
            codecProduct fields

        TContainer container ->
            codecContainer container

        _ ->
            CG.unit


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> Type -> Expression
codecTypeField name l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic
                |> codecField name

        TNamed named ->
            CG.string "codecTypeFieldTNamed"

        TProduct fields ->
            codecProduct fields
                |> codecField name

        TContainer container ->
            codecContainerField name container

        TFunction arg res ->
            CG.unit


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
            CG.apply [ codecFn "list", codecType l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ codecFn "set", codecType l1Type ]
                |> CG.parens

        CDict _ l1valType ->
            CG.apply [ codecFn "dict", codecType l1valType ]
                |> CG.parens

        COptional l1Type ->
            CG.apply [ codecFn "maybe", codecType l1Type ]
                |> CG.parens


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
                |> CG.pipe
                    (CG.apply
                        [ codecFn "object"
                        , CG.fun typeName
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
    CG.string "codecProduct"


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Codec.optionalField`.
-}
codecContainerField : String -> Container -> Expression
codecContainerField name container =
    case container of
        CList l1Type ->
            CG.apply [ codecFn "list", codecType l1Type ]
                |> CG.parens
                |> codecField name

        CSet l1Type ->
            CG.apply [ codecFn "set", codecType l1Type ]
                |> CG.parens
                |> codecField name

        CDict _ l1valType ->
            CG.apply [ codecFn "dict", codecType l1valType ]
                |> CG.parens
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
        [ CG.apply
            [ codecFn "buildObject"
            ]
        ]
        fields


{-| Helper function for building field codecs.
-}
codecField : String -> Expression -> Expression
codecField name expr =
    CG.apply
        [ codecFn "field"
        , CG.string (Case.toCamelCaseLower name)
        , CG.accessFun ("." ++ Case.toCamelCaseLower name)
        , expr
        ]


{-| Helper function for building optional field codecs.
-}
codecOptionalField : String -> Expression -> Expression
codecOptionalField name expr =
    CG.apply
        [ codecFn "optionalField"
        , CG.string (Case.toCamelCaseLower name)
        , CG.accessFun ("." ++ Case.toCamelCaseLower name)
        , expr
        ]


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


codecMod : List String
codecMod =
    [ "Codec" ]


enumMod : List String
enumMod =
    [ "Enum" ]


codecFn : String -> Expression
codecFn =
    CG.fqFun codecMod


codecImport : Import
codecImport =
    CG.importStmt codecMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Codec" ])


setImport : Import
setImport =
    CG.importStmt [ "Set" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Set" ])


dictImport : Import
dictImport =
    CG.importStmt [ "Dict" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Dict" ])


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])
