module Templates.L1 exposing (..)

import ElmDSL exposing (..)
import LevelOne exposing (..)


unit : String -> ( Declaration, ImportsAndExposing )
unit name =
    ( functionDeclaration Nothing Nothing name [] unitExpr, emptyImportsAndExposing )


typeDecl : String -> Declarable -> ( Declaration, ImportsAndExposing )
typeDecl name decl =
    case decl of
        DAlias _ ->
            unit name

        DSum _ ->
            unit name

        DRestricted _ ->
            unit name
