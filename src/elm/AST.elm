module AST exposing (AST, Block(..), example, pretty, prettyToList)

import Array exposing (Array)
import Pretty exposing (Doc)
import Random exposing (Seed)


type alias AST =
    List Block


type Block
    = Statement (List String)
    | Blocks (List Block)
    | SubBlock Block


pretty : AST -> String
pretty ast =
    prettyAst ast
        |> Pretty.pretty 40


prettyToList : AST -> List String
prettyToList _ =
    [ "prettyToList" ]


example : Int -> Seed -> ( AST, Seed )
example count seed =
    let
        exampleInner : Int -> ( AST, Seed ) -> ( AST, Seed )
        exampleInner innerCount ( acc, innerSeed ) =
            case innerCount of
                0 ->
                    ( acc, innerSeed )

                c ->
                    let
                        ( bl, innerSeed2 ) =
                            exampleBlock innerSeed
                    in
                    exampleInner (innerCount - 1) ( bl :: acc, innerSeed2 )

        ( blocks, seed2 ) =
            exampleInner count ( [], seed )
    in
    ( blocks, seed2 )


exampleBlock : Seed -> ( Block, Seed )
exampleBlock seed =
    let
        ( choice, seed2 ) =
            Random.step (Random.uniform 1 [ 2, 3 ]) seed
    in
    case choice of
        1 ->
            exampleStatement seed2

        2 ->
            Tuple.mapFirst Blocks (Tuple.mapFirst List.singleton (exampleStatement seed2))

        _ ->
            Tuple.mapFirst SubBlock (exampleStatement seed2)


exampleStatement : Seed -> ( Block, Seed )
exampleStatement seed =
    let
        ( count, seed2 ) =
            Random.step (Random.int 3 12) seed

        ( wordList, seed3 ) =
            Random.step
                (Random.int 0 (Array.length words)
                    |> Random.map (\index -> Array.get index words |> Maybe.withDefault "default")
                    |> Random.list count
                )
                seed2
    in
    ( Statement wordList, seed3 )


words : Array String
words =
    [ "if", "then", "array", "let", "in", "begin", "end", "case", "where", "sum", "prod", "list", "cons" ]
        |> Array.fromList


prettyAst : AST -> Doc
prettyAst ast =
    List.map prettyBlock ast
        |> Pretty.lines


prettyBlock : Block -> Doc
prettyBlock block =
    case block of
        Statement expr ->
            List.map Pretty.string expr
                |> Pretty.softlines

        Blocks blocks ->
            Pretty.string "{"
                |> Pretty.a Pretty.line
                |> Pretty.a (prettyAst blocks |> Pretty.indent 4)
                |> Pretty.a Pretty.line
                |> Pretty.a (Pretty.string "}")

        SubBlock subBlock ->
            prettyBlock subBlock
                |> Pretty.indent 4
