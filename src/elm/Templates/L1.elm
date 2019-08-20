module Templates.L1 exposing (..)

import Codec
import ElmDSL exposing (..)
import LevelOne exposing (..)
import String.Case as Case



--== Type Declarations


typeDecl : String -> Declarable -> ( Declaration, ImportsAndExposing )
typeDecl name decl =
    case decl of
        DAlias l1Type ->
            typeAlias name l1Type

        DSum constructors ->
            customType name constructors

        DRestricted _ ->
            dummy name


typeAlias : String -> Type -> ( Declaration, ImportsAndExposing )
typeAlias name l1Type =
    ( aliasDeclaration Nothing (Case.toCamelCaseUpper name) [] (lowerType l1Type)
    , emptyImportsAndExposing
    )


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


lowerProduct : List ( String, Type ) -> TypeAnnotation
lowerProduct fields =
    let
        mappedFields =
            List.map
                (Tuple.mapBoth Case.toCamelCaseUpper lowerType)
                fields
    in
    record mappedFields


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


codec : String -> Declarable -> ( Declaration, ImportsAndExposing )
codec name decl =
    case decl of
        DAlias l1Type ->
            typeAliasCodec name l1Type

        DSum constructors ->
            customTypeCodec name constructors

        DRestricted _ ->
            dummy (name ++ "Codec")


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
            operatorApplication "|>"
                left
                (application
                    [ functionOrValue [ "Codec" ] "object"
                    , functionOrValue [] typeName
                    ]
                )
                (application
                    [ functionOrValue [ "Codec" ] "buildObject"
                    ]
                )
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


codecType : Type -> Expression
codecType l1Type =
    case l1Type of
        TBasic basic ->
            codecBasic basic

        TNamed name ->
            -- genericType (Case.toCamelCaseUpper name)
            unitExpr

        TProduct fields ->
            codecProduct fields

        TContainer container ->
            codecContainer container

        TFunction arg res ->
            unitExpr


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


codecProduct : List ( String, Type ) -> Expression
codecProduct fields =
    let
        mappedFields =
            List.map
                (Tuple.mapBoth Case.toCamelCaseUpper codecType)
                fields
    in
    --record mappedFields
    unitExpr


codecContainer : Container -> Expression
codecContainer container =
    case container of
        CList l1Type ->
            application [ functionOrValue [ "Codec" ] "list", codecType l1Type ]

        CSet l1Type ->
            application [ functionOrValue [ "Codec" ] "set", codecType l1Type ]

        CDict l1keyType l1valType ->
            application [ functionOrValue [ "Codec" ] "dict", codecType l1keyType, codecType l1valType ]

        COptional l1Type ->
            application [ functionOrValue [ "Codec" ] "maybe", codecType l1Type ]



--== Helper Functions


dummy : String -> ( Declaration, ImportsAndExposing )
dummy name =
    ( functionDeclaration Nothing Nothing name [] unitExpr, emptyImportsAndExposing )


codecMod : List String
codecMod =
    [ "Codec" ]
