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
import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, Linkage, TypeAnnotation)
import L1 exposing (Basic(..), Container(..), Declarable(..), Restricted(..), Type(..))
import L2 exposing (Flagged(..))
import Maybe.Extra
import Set exposing (Set)
import Templates.Util as Util



--== Type Declarations


{-| Turns an L1 type declaration into Elm code.

A type can result in a list of declarations - enums in addition to declaring a
type can also declare the permitted enum values.

-}
typeDecl : String -> Comment DocComment -> Declarable Flagged -> ( List Declaration, Linkage )
typeDecl name doc decl =
    case decl of
        DAlias l1Type ->
            typeAlias name (Just doc) l1Type
                |> Tuple.mapFirst List.singleton

        DSum constructors ->
            customType name (Just doc) constructors
                |> Tuple.mapFirst List.singleton

        DEnum labels ->
            enumCustomType name (Just doc) labels

        DRestricted res ->
            restrictedType name (Just doc) res


{-| Turns an L1 restricted type into Elm code.

This will result in a list of declarations - the type declaration in addition
to the functions needed to create or unbox the restricted type.

-}
restrictedType : String -> Maybe (Comment DocComment) -> Restricted -> ( List Declaration, Linkage )
restrictedType name maybeDoc restricted =
    case restricted of
        RInt res ->
            restrictedInt name maybeDoc res

        RString res ->
            restrictedString name maybeDoc res


restrictedInt :
    String
    -> Maybe (Comment DocComment)
    -> { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    -> ( List Declaration, Linkage )
restrictedInt name maybeDoc res =
    let
        minGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun refinedMod "gte", CG.int minValue ])
                res.min

        maxGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun refinedMod "lte", CG.int maxValue ])
                res.max

        guards =
            [ minGuard, maxGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an int.
            typeAlias name Nothing (BInt |> TBasic)
                |> Tuple.mapFirst List.singleton

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl maybeDoc (Util.safeCCU name) [] [ ( Util.safeCCU name, [ CG.intAnn ] ) ]

                restrictedSig =
                    CG.typed "Refined"
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
                    CG.applyBinOp
                        (Util.mChainResult (CG.apply [ gd, CG.val "val" ])
                            (List.map CG.parens gds)
                        )
                        CG.piper
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Util.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun refinedMod "define"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "int"
                        , CG.fqVal encodeMod "int"
                        , CG.fqFun refinedMod "intErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl maybeDoc (Just restrictedSig) (Util.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , CG.emptyLinkage
                |> CG.addImport (guardedImportExposing [ "Refined", "IntError" ])
                |> CG.addImport decodeImport
                |> CG.addImport encodeImport
                |> CG.addExposing (CG.funExpose (Util.safeCCL name))
                |> CG.addExposing (CG.typeOrAliasExpose (Util.safeCCU name))
            )


restrictedString :
    String
    -> Maybe (Comment DocComment)
    -> { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }
    -> ( List Declaration, Linkage )
restrictedString name maybeDoc res =
    let
        minLenGuard =
            Maybe.map
                (\minValue -> CG.apply [ CG.fqFun refinedMod "minLength", CG.int minValue ])
                res.minLength

        maxLenGuard =
            Maybe.map
                (\maxValue -> CG.apply [ CG.fqFun refinedMod "maxLength", CG.int maxValue ])
                res.maxLength

        patternGuard =
            Maybe.map
                (\regex -> CG.apply [ CG.fqFun refinedMod "regexMatch", CG.string regex ])
                res.regex

        guards =
            [ minLenGuard, maxLenGuard, patternGuard ] |> Maybe.Extra.values
    in
    case guards of
        [] ->
            -- If there are no guard clauses, it is just an string.
            typeAlias name Nothing (BString |> TBasic)
                |> Tuple.mapFirst List.singleton

        gd :: gds ->
            let
                boxedTypeDecl =
                    CG.customTypeDecl maybeDoc (Util.safeCCU name) [] [ ( Util.safeCCU name, [ CG.stringAnn ] ) ]

                restrictedSig =
                    CG.typed "Refined"
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
                    CG.applyBinOp
                        (Util.mChainResult (CG.apply [ gd, CG.val "val" ])
                            (List.map CG.parens gds)
                        )
                        CG.piper
                        typeWrapper
                        |> CG.letFunction "guardFn" [ CG.varPattern "val" ]

                unboxFn =
                    CG.letFunction "unboxFn"
                        [ CG.namedPattern (Util.safeCCU name) [ CG.varPattern "val" ] ]
                        (CG.val "val")

                restrictedImpl =
                    CG.apply
                        [ CG.fqFun refinedMod "define"
                        , CG.fun "guardFn"
                        , CG.fqVal decodeMod "string"
                        , CG.fqVal encodeMod "string"
                        , CG.fqFun refinedMod "stringErrorToString"
                        , CG.fun "unboxFn"
                        ]
                        |> CG.letExpr [ guardFn, unboxFn ]

                restrictedDecl =
                    CG.valDecl maybeDoc (Just restrictedSig) (Util.safeCCL name) restrictedImpl
            in
            ( [ boxedTypeDecl
              , restrictedDecl
              ]
            , CG.emptyLinkage
                |> CG.addImport (guardedImportExposing [ "Refined", "StringError" ])
                |> CG.addImport decodeImport
                |> CG.addImport encodeImport
                |> CG.addExposing (CG.funExpose (Util.safeCCL name))
                |> CG.addExposing (CG.typeOrAliasExpose (Util.safeCCU name))
            )


{-| Turns an L1 `Type` into a type alias in Elm code.
-}
typeAlias : String -> Maybe (Comment DocComment) -> Type Flagged -> ( Declaration, Linkage )
typeAlias name maybeDoc l1Type =
    let
        ( loweredType, linkage ) =
            lowerType l1Type
    in
    ( CG.aliasDecl maybeDoc (Util.safeCCU name) [] loweredType
    , linkage
        |> CG.addExposing (CG.typeOrAliasExpose (Util.safeCCU name))
    )


{-| Turns an L1 sum type into a custom type in Elm code.
-}
customType : String -> Maybe (Comment DocComment) -> List ( String, List ( String, Type Flagged ) ) -> ( Declaration, Linkage )
customType name maybeDoc constructors =
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
    ( CG.customTypeDecl maybeDoc (Util.safeCCU name) [] mappedConstructors
    , CG.combineLinkage linkages
        |> CG.addExposing (CG.openTypeExpose (Util.safeCCU name))
    )


{-| Turns an L1 enum type into a custom type in Elm code.

This produces 2 declarations, one for the guarded type, and one for the enum
declaring its allowed values.

-}
enumCustomType : String -> Maybe (Comment DocComment) -> List String -> ( List Declaration, Linkage )
enumCustomType name maybeDoc labels =
    let
        constructors =
            List.map
                (\label -> ( Util.safeCCU name ++ Util.safeCCU label, [] ))
                labels

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "define"
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
    ( [ CG.customTypeDecl maybeDoc (Util.safeCCU name) [] constructors
      , CG.valDecl maybeDoc (Just enumSig) (Util.safeCCL name) enumValues
      ]
    , CG.emptyLinkage
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose (Util.safeCCL name))
        |> CG.addExposing (CG.openTypeExpose (Util.safeCCU name))
    )


{-| Turns an L1 enum type into a refined type in Elm code.

This produces 2 declarations, one for the refined type, and one for the enum
declaring its allowed values.

-}
enumRefinedType : String -> List String -> ( List Declaration, Linkage )
enumRefinedType name labels =
    let
        guardedConstructor =
            [ ( Util.safeCCU name, [ CG.stringAnn ] ) ]

        enumValues =
            CG.apply
                [ CG.fqFun enumMod "define"
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
    , CG.emptyLinkage
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose (Util.safeCCL name))
    )


{-| Lowers an L1 type into an Elm type annotation.
-}
lowerType : Type Flagged -> ( TypeAnnotation, Linkage )
lowerType l1Type =
    case l1Type of
        TUnit ->
            ( CG.unitAnn, CG.emptyLinkage )

        TBasic basic ->
            ( lowerBasic basic
            , CG.emptyLinkage
            )

        TNamed name _ ->
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
lowerProduct : List ( String, Type Flagged ) -> ( TypeAnnotation, Linkage )
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
lowerContainer : Container Flagged -> ( TypeAnnotation, Linkage )
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
            lowerDict l1keyType l1valType

        COptional l1Type ->
            lowerType l1Type
                |> Tuple.mapFirst CG.maybeAnn


lowerDict : Type Flagged -> Type Flagged -> ( TypeAnnotation, Linkage )
lowerDict l1keyType l1valType =
    case l1keyType of
        TNamed name (FlRestricted basic) ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.fqTyped dictRefinedMod "Dict" [ lowerBasic basic, keyAnn, valAnn ]
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictRefinedImport
            )

        TNamed name FlEnum ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.fqTyped dictEnumMod "Dict" [ keyAnn, valAnn ]
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictEnumImport
            )

        _ ->
            let
                ( keyAnn, keyLink ) =
                    lowerType l1keyType

                ( valAnn, valLink ) =
                    lowerType l1valType
            in
            ( CG.dictAnn keyAnn valAnn
            , CG.combineLinkage [ keyLink, valLink ] |> CG.addImport dictImport
            )


{-| Lowers an L1 function type into an Elm type annotation
-}
lowerFun : Type Flagged -> Type Flagged -> ( TypeAnnotation, Linkage )
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
codec : String -> Declarable Flagged -> ( Declaration, Linkage )
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
typeAliasCodec : String -> Type Flagged -> ( Declaration, Linkage )
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

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( CG.funDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


{-| Generates a Codec for an L1 sum type.
-}
customTypeCodec : String -> List ( String, List ( String, Type Flagged ) ) -> ( Declaration, Linkage )
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

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( CG.funDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addExposing (CG.funExpose codecFnName)
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

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( CG.funDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose codecFnName)
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
                , CG.parens (CG.apply [ CG.fqFun refinedMod "encoder", CG.val enumName ])
                , CG.parens (CG.apply [ CG.fqFun refinedMod "decoder", CG.val enumName ])
                ]

        doc =
            CG.emptyDocComment
                |> CG.markdown ("Codec for " ++ typeName ++ ".")
    in
    ( CG.funDecl
        (Just doc)
        (Just sig)
        codecFnName
        []
        impl
    , CG.emptyLinkage
        |> CG.addImport codecImport
        |> CG.addImport enumImport
        |> CG.addExposing (CG.funExpose codecFnName)
    )


codecCustomType : List ( String, List ( String, Type Flagged ) ) -> Expression
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


codecMatchFn : List ( String, List ( String, Type Flagged ) ) -> Expression
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
codecNamedType : String -> Type Flagged -> Expression
codecNamedType name l1Type =
    case l1Type of
        TUnit ->
            codecUnit

        TBasic basic ->
            codecType l1Type

        TNamed named _ ->
            CG.string "codecNamedType_TNamed"

        TProduct fields ->
            codecNamedProduct name fields

        TContainer container ->
            codecType l1Type

        TFunction arg res ->
            CG.unit


{-| Generates a Codec for an L1 type.
-}
codecType : Type Flagged -> Expression
codecType l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic

        TNamed named _ ->
            codecNamed named

        TProduct fields ->
            codecProduct fields

        TContainer container ->
            codecContainer container

        _ ->
            CG.unit


{-| Generates a field codec for a named field with an L1 type.
-}
codecTypeField : String -> Type Flagged -> Expression
codecTypeField name l1Type =
    case l1Type of
        TUnit ->
            codecUnit |> codecField name

        TBasic basic ->
            codecBasic basic
                |> codecField name

        TNamed named _ ->
            codecNamed named
                |> codecField name

        TProduct fields ->
            codecProduct fields
                |> codecField name

        TContainer container ->
            codecContainerField name container

        TFunction arg res ->
            CG.unit


{-| Generates a codec for unit.

Decodes `()`, and encodes to JSON `null`.

-}
codecUnit =
    CG.apply
        [ codecFn "constant"
        , CG.unit
        ]


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


codecContainer : Container Flagged -> Expression
codecContainer container =
    case container of
        CList l1Type ->
            CG.apply [ codecFn "list", codecType l1Type ]
                |> CG.parens

        CSet l1Type ->
            CG.apply [ codecFn "set", codecType l1Type ]
                |> CG.parens

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType

        COptional l1Type ->
            CG.apply [ codecFn "maybe", codecType l1Type ]
                |> CG.parens


codecDict : Type Flagged -> Type Flagged -> Expression
codecDict l1keyType l1valType =
    case l1keyType of
        TNamed name (FlRestricted basic) ->
            let
                _ =
                    Debug.log "codecDict" "Codec for dict with restricted key."
            in
            CG.apply
                [ codecFn "build"
                , CG.apply
                    [ CG.fqFun refinedMod "dictEncoder"
                    , CG.val (Util.safeCCL name)
                    , CG.apply [ codecFn "encoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun refinedMod "dictDecoder"
                    , CG.val (Util.safeCCL name)
                    , CG.apply [ codecFn "decoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        TNamed name FlEnum ->
            let
                _ =
                    Debug.log "codecDict" "Codec for dict with enum key."
            in
            CG.apply
                [ codecFn "build"
                , CG.apply
                    [ CG.fqFun enumMod "dictEncoder"
                    , CG.val (Util.safeCCL name)
                    , CG.apply [ codecFn "encoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                , CG.apply
                    [ CG.fqFun enumMod "dictDecoder"
                    , CG.val (Util.safeCCL name)
                    , CG.apply [ codecFn "decoder", codecType l1valType ]
                        |> CG.parens
                    ]
                    |> CG.parens
                ]

        _ ->
            CG.apply [ codecFn "dict", codecType l1valType ]
                |> CG.parens


{-| Generates a codec for an L1 product type that has been named as an alias.
The alias name is also the constructor function for the type.
-}
codecNamedProduct : String -> List ( String, Type Flagged ) -> Expression
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
codecProduct : List ( String, Type Flagged ) -> Expression
codecProduct fields =
    CG.string "codecProduct"


{-| Generates a field codec for an L1 container type. The 'optional' type is mapped
onto `Maybe` and makes use of `Codec.optionalField`.
-}
codecContainerField : String -> Container Flagged -> Expression
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

        CDict l1keyType l1valType ->
            codecDict l1keyType l1valType
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
        , CG.string name
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


dictEnumMod : List String
dictEnumMod =
    [ "Dict", "Enum" ]


enumMod : List String
enumMod =
    [ "Enum" ]


maybeMod : List String
maybeMod =
    [ "Maybe" ]


dictRefinedMod : List String
dictRefinedMod =
    [ "Dict", "Refined" ]


refinedMod : List String
refinedMod =
    [ "Refined" ]


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


dictEnumImport : Import
dictEnumImport =
    CG.importStmt dictEnumMod Nothing Nothing


refinedImport : Import
refinedImport =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit [ CG.typeOrAliasExpose "Refined" ])


dictRefinedImport : Import
dictRefinedImport =
    CG.importStmt dictRefinedMod Nothing Nothing


guardedImportExposing : List String -> Import
guardedImportExposing exposings =
    CG.importStmt refinedMod Nothing (Just <| CG.exposeExplicit (List.map CG.typeOrAliasExpose exposings))
