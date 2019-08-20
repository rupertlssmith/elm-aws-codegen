module Templates.L1 exposing (..)

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



--== Decoders
--== Encoders
--== Helper Functions


dummy : String -> ( Declaration, ImportsAndExposing )
dummy name =
    ( functionDeclaration Nothing Nothing (Case.toCamelCaseUpper name) [] unitExpr, emptyImportsAndExposing )
