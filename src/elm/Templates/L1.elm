module Templates.L1 exposing
    ( typeDecl, codec
    , lowerType, lowerFun
    )

{-| Elm code generation from L1 models.

Type declarations:

@docs typeDecl, codec

Lowerings of L1 into Elm type annotations:

@docs lowerType, lowerFun

-}

import Codec
import Elm.CodeGen as CG exposing (Declaration, Expression, Import, Linkage, TypeAnnotation)
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import Maybe.Extra
import Set exposing (Set)
import Templates.Util as Util



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
            -- enumGuardedType name labels
            enumCustomType name labels

        DRestricted res ->
            restrictedType name res


{-| Turns an L1 restricted type into Elm code.

This will result in a list of declarations - the type declaration in addition
to the functions needed to create or unbox the restricted type.

-}
restrictedType : String -> Restricted -> ( List Declaration, List Linkage )
restrictedType name restricted =
    case restricted of
        RInt res ->
            restrictedInt name res

        RString res ->
            restrictedString name res


restrictedInt :
    String
    -> { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    -> ( List Declaration, List Linkage )
restrictedInt name res =
    let
        minGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun guardedMod "gt", CG.int minValue ])
                res.min

        maxGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun guardedMod "lt", CG.int maxValue ])
                res.max

        guards =
            [ minGuard, maxGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an int.
            typeAlias name (BInt |> TBasic)
                |> Tuple.mapBoth List.singleton List.singleton

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl Nothing (Util.safeCCU name) [] [ ( Util.safeCCU name, [ CG.intAnn ] ) ]

                restrictedSig =
                    CG.typed "Guarded"
                        [ CG.typed "Int" []
                        , CG.typed (Util.safeCCU name) []
                        , CG.typed "IntError" []
                        ]

                typeWrapper =
                    CG.apply
                        [ CG.fqFun resultMod "map"
                        , CG.fun (Util.safeCCU name)
                        ]

                guardFn =
                    CG.opApply "|>"
                        CG.infixLeft
                        (Util.mChainResult (CG.apply [ gd, CG.val "val" ])
                            (List.map CG.parens gds)
                        )
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Util.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun guardedMod "make"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "int"
                        , CG.fqVal encodeMod "int"
                        , CG.fqFun guardedMod "intErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl Nothing (Just restrictedSig) (Util.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , [ CG.emptyLinkage
                    |> CG.addImport (guardedImportExposing [ "Guarded", "IntError" ])
                    |> CG.addImport decodeImport
                    |> CG.addImport encodeImport
              ]
            )


restrictedString :
    String
    -> { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }
    -> ( List Declaration, List Linkage )
restrictedString name res =
    let
        minLenGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun guardedMod "minLength", CG.int minValue ])
                res.minLength

        maxLenGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun guardedMod "maxLength", CG.int maxValue ])
                res.maxLength

        patternGuard =
            Maybe.map
                (\regex -> CG.apply [ CG.fqFun guardedMod "regexMatch", CG.string regex ])
                res.regex

        guards =
            [ minLenGuard, maxLenGuard, patternGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an string.
            typeAlias name (BString |> TBasic)
                |> Tuple.mapBoth List.singleton List.singleton

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl Nothing (Util.safeCCU name) [] [ ( Util.safeCCU name, [ CG.stringAnn ] ) ]

                restrictedSig =
                    CG.typed "Guarded"
                        [ CG.typed "String" []
                        , CG.typed (Util.safeCCU name) []
                        , CG.typed "StringError" []
                        ]

                typeWrapper =
                    CG.apply
                        [ CG.fqFun resultMod "map"
                        , CG.fun (Util.safeCCU name)
                        ]

                guardFn =
                    CG.opApply "|>"
                        CG.infixLeft
                        (CG.pipe (CG.apply [ gd, CG.val "val" ])
                            (List.map (\expr -> CG.apply [ CG.fqFun resultMod "andThen", CG.parens expr ]) gds)
                        )
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Util.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun guardedMod "make"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "string"
                        , CG.fqVal encodeMod "string"
                        , CG.fqFun guardedMod "stringErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl Nothing (Just restrictedSig) (Util.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , [ CG.emptyLinkage
                    |> CG.addImport (guardedImportExposing [ "Guarded", "StringError" ])
                    |> CG.addImport decodeImport
                    |> CG.addImport encodeImport
              ]
            )


{-| Turns an L1 `Type` into a type alias in Elm code.
-}
typeAlias : String -> Type -> ( Declaration, Linkage )
typeAlias name l1Type =
    let
        ( loweredType, linkage ) =
            lowerType l1Type
    in
    ( CG.aliasDecl Nothing (Util.safeCCU name) [] loweredType
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
                    ( ( Util.safeCCU consName, loweredArgs ), linkage )
                )
                constructors
                |> List.unzip
    in
    ( CG.customTypeDecl Nothing (Util.safeCCU name) [] mappedConstructors
    , CG.combineLinkage linkages
    )


{-| Turns an L1 enum type into a custom type in Elm code.

This produces 2 declarations, one for the guarded type, and one for the enum
declaring its allowed values.

-}
enumCustomType : String -> List String -> ( List Declaration, List Linkage )
enumCustomType name labels =
    let
        constructors =
            List.map
                (\label -> ( Util.safeCCU name ++ Util.safeCCU label, [] ))
                labels

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "make"
                , List.map
                    (\label ->
                        CG.fun (Util.safeCCU name ++ Util.safeCCU label)
                    )
                    labels
                    |> CG.list
                , CG.lambda [ CG.varPattern "val" ]
                    (CG.caseExpr (CG.val "val")
                        (List.map
                            (\label ->
                                ( CG.namedPattern (Util.safeCCU name ++ Util.safeCCU label) []
                                , CG.string label
                                )
                            )
                            labels
                        )
                    )
                ]

        enumSig =
            CG.typed "Enum" [ CG.typed (Util.safeCCU name) [] ]
    in
    ( [ CG.customTypeDecl Nothing (Util.safeCCU name) [] constructors
      , CG.valDecl Nothing (Just enumSig) (Util.safeCCL name) enumValues
      ]
    , [ CG.emptyLinkage |> CG.addImport enumImport ]
    )


{-| Turns an L1 enum type into an opaque guarded type in Elm code.

This produces 2 declarations, one for the guarded type, and one for the enum
declaring its allowed values.

-}
enumGuardedType : String -> List String -> ( List Declaration, List Linkage )
enumGuardedType name labels =
    let
        guardedConstructor =
            [ ( Util.safeCCU name, [ CG.stringAnn ] ) ]

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "make"
                , List.map
                    (\label ->
                        CG.apply
                            [ CG.fun (Util.safeCCU name)
                            , label |> CG.string
                            ]
                    )
                    labels
                    |> CG.list
                , CG.lambda [ CG.namedPattern (Util.safeCCU name) [ CG.varPattern "val" ] ]
                    (CG.val "val")
                ]

        enumSig =
            CG.typed "Enum" [ CG.typed (Util.safeCCU name) [] ]
    in
    ( [ CG.customTypeDecl Nothing (Util.safeCCU name) [] guardedConstructor
      , CG.valDecl Nothing (Just enumSig) (Util.safeCCL name) enumValues
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
            ( CG.typed (Util.safeCCU name) []
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
                    ( ( Util.safeCCL name, loweredType ), linkage )
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


{-| Lowers an L1 function type into an Elm type annotation
-}
lowerFun : Type -> Type -> ( TypeAnnotation, Linkage )
lowerFun fromType toType =
    let
        ( from, fromLinkage ) =
            lowerType fromType

        ( to, toLinkage ) =
            lowerType toType
    in
    ( CG.funAnn from to
    , CG.combineLinkage [ fromLinkage, toLinkage ]
    )



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

        DRestricted res ->
            restrictedCodec name res


{-| Generates a Codec for an L1 type alias.
-}
typeAliasCodec : String -> Type -> ( Declaration, Linkage )
typeAliasCodec name l1Type =
    let
        codecFnName =
            Util.safeCCL (name ++ "Codec")

        typeName =
            Util.safeCCU name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

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
            Util.safeCCL (name ++ "Codec")

        typeName =
            Util.safeCCU name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

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
    let
        codecFnName =
            Util.safeCCL (name ++ "Codec")

        typeName =
            Util.safeCCU name

        enumName =
            Util.safeCCL name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun codecMod "build"
                , CG.parens (CG.apply [ CG.fqFun enumMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun enumMod "decoder", CG.val enumName ])
                ]
    in
    ( CG.funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
    )


restrictedCodec : String -> Restricted -> ( Declaration, Linkage )
restrictedCodec name _ =
    let
        codecFnName =
            Util.safeCCL (name ++ "Codec")

        typeName =
            Util.safeCCU name

        enumName =
            Util.safeCCL name

        sig =
            CG.typed "Codec" [ CG.typed typeName [] ]

        impl =
            CG.apply
                [ CG.fqFun codecMod "build"
                , CG.parens (CG.apply [ CG.fqFun guardedMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun guardedMod "decoder", CG.val enumName ])
                ]
    in
    ( CG.funDecl
        (Just <| "{-| Codec for " ++ typeName ++ ". -}")
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
    )


codecCustomType : List ( String, List ( String, Type ) ) -> Expression
codecCustomType constructors =
    let
        codecVariant name args =
            List.foldr
                (\( _, l1Type ) accum -> codecType l1Type :: accum)
                [ Util.safeCCU name |> CG.fun
                , Util.safeCCU name |> CG.string
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
            "f" ++ Util.safeCCL name

        args =
            List.foldr (\( name, _ ) accum -> (consFnName name |> CG.varPattern) :: accum)
                [ CG.varPattern "value" ]
                constructors

        consPattern ( name, consArgs ) =
            ( CG.namedPattern (Util.safeCCU name)
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
            codecNamed named

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
            codecNamed named
                |> codecField name

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


codecNamed named =
    CG.fun (Util.safeCCL (named ++ "Codec"))


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
            Util.safeCCU name

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
        , CG.string name
        , CG.accessFun ("." ++ Util.safeCCL name)
        , expr
        ]


{-| Helper function for building optional field codecs.
-}
codecOptionalField : String -> Expression -> Expression
codecOptionalField name expr =
    CG.apply
        [ codecFn "optionalField"
        , CG.string (Util.safeCCL name)
        , CG.accessFun ("." ++ Util.safeCCL name)
        , expr
        ]


dummyFn : String -> ( Declaration, Linkage )
dummyFn name =
    ( CG.funDecl Nothing Nothing name [] CG.unit, CG.emptyLinkage )


codecMod : List String
codecMod =
    [ "Codec" ]


decodeMod : List String
decodeMod =
    [ "Json", "Decode" ]


encodeMod : List String
encodeMod =
    [ "Json", "Encode" ]


enumMod : List String
enumMod =
    [ "Enum" ]


maybeMod : List String
maybeMod =
    [ "Maybe" ]


guardedMod : List String
guardedMod =
    [ "Guarded" ]


resultMod : List String
resultMod =
    [ "Result" ]


codecFn : String -> Expression
codecFn =
    CG.fqFun codecMod


codecImport : Import
codecImport =
    CG.importStmt codecMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Codec" ])


decodeImport : Import
decodeImport =
    CG.importStmt decodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Decoder" ])


encodeImport : Import
encodeImport =
    CG.importStmt encodeMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Value" ])


setImport : Import
setImport =
    CG.importStmt [ "Set" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Set" ])


dictImport : Import
dictImport =
    CG.importStmt [ "Dict" ] Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Dict" ])


enumImport : Import
enumImport =
    CG.importStmt enumMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Enum" ])


guardedImportExposing : List String -> Import
guardedImportExposing exposings =
    CG.importStmt guardedMod Nothing (Just <| CG.exposeExplicit (List.map CG.typeOrAliasExpose exposings))
