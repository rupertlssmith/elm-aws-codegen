module ElmDSL exposing (..)

import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range, emptyRange)



--== Modules


normalModule : ModuleName -> List TopLevelExpose -> Module
normalModule name exposes =
    let
        exposingList =
            case exposes of
                [] ->
                    All emptyRange

                es ->
                    Explicit (List.map (\exp -> Node emptyRange exp) exposes)
    in
    NormalModule
        { moduleName = Node emptyRange name
        , exposingList = Node emptyRange exposingList
        }
