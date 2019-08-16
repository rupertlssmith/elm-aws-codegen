module ElmDSL exposing (..)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range, emptyRange)



--== Files


file : Module -> List Import -> List Declaration -> List Comment -> File
file mod imports declarations comments =
    { moduleDefinition = nodify mod
    , imports = nodifyAll imports
    , declarations = nodifyAll declarations
    , comments = nodifyAll comments
    }



--== Modules


normalModule : ModuleName -> List TopLevelExpose -> Module
normalModule name exposes =
    let
        exposingList =
            case exposes of
                [] ->
                    All emptyRange

                es ->
                    Explicit <| nodifyAll exposes
    in
    NormalModule
        { moduleName = nodify name
        , exposingList = nodify exposingList
        }


nodify : a -> Node a
nodify exp =
    Node emptyRange exp


nodifyAll : List a -> List (Node a)
nodifyAll =
    List.map nodify
