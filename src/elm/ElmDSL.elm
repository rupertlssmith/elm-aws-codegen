module ElmDSL exposing (..)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))



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



--== Imports


import_ : ModuleName -> Maybe ModuleName -> Maybe Exposing -> Import
import_ name alias exposes =
    { moduleName = nodify name
    , moduleAlias = nodifyMaybe alias
    , exposingList = nodifyMaybe exposes
    }



--== Types


genericType : String -> TypeAnnotation
genericType name =
    GenericType name


typed : ModuleName -> String -> List TypeAnnotation -> TypeAnnotation
typed moduleName name args =
    Typed (nodify ( moduleName, name )) (nodifyAll args)


unit : TypeAnnotation
unit =
    Unit


tupled : List TypeAnnotation -> TypeAnnotation
tupled args =
    Tupled (nodifyAll args)


record : RecordDefinition -> TypeAnnotation
record recDef =
    Record recDef


genericRecord : String -> RecordDefinition -> TypeAnnotation
genericRecord argName recDef =
    GenericRecord (nodify argName) (nodify recDef)


functionTypeAnnotation : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
functionTypeAnnotation arg result =
    FunctionTypeAnnotation (nodify arg) (nodify result)



--== Functions (and Values)


signature : String -> TypeAnnotation -> Signature
signature name annotation =
    { name = nodify name
    , typeAnnotation = nodify annotation
    }


functionDeclaration : Maybe Documentation -> Maybe Signature -> FunctionImplementation -> Declaration
functionDeclaration documentation sig implementation =
    FunctionDeclaration
        { documentation = nodifyMaybe documentation
        , signature = nodifyMaybe sig
        , declaration = nodify implementation
        }



--== Expressions


{-| UnitExpr
-}
unitExpr : Expression
unitExpr =
    UnitExpr


{-| Application (List (Node Expression))
-}
application : List Expression -> Expression
application exprs =
    Application (nodifyAll exprs)


{-| OperatorApplication String InfixDirection (Node Expression) (Node Expression)
-}
operatorApplication : String -> InfixDirection -> Expression -> Expression -> Expression
operatorApplication symbol infixDir exprl exprr =
    OperatorApplication symbol infixDir (nodify exprl) (nodify exprr)


{-| FunctionOrValue ModuleName String
-}
functionOrValue : ModuleName -> String -> Expression
functionOrValue moduleName name =
    FunctionOrValue moduleName name


{-| IfBlock (Node Expression) (Node Expression) (Node Expression)
-}
ifBlock : Expression -> Expression -> Expression -> Expression
ifBlock boolExpr trueExpr falseExpr =
    IfBlock (nodify boolExpr) (nodify trueExpr) (nodify falseExpr)


{-| PrefixOperator String
-}
prefixOperator : String -> Expression
prefixOperator symbol =
    PrefixOperator symbol


{-| Operator String
-}
operator : String -> Expression
operator symbol =
    Operator symbol


{-| Integer Int
-}
integer : Int -> Expression
integer val =
    Integer val


{-| Hex Int
-}
hex : Int -> Expression
hex val =
    Hex val


{-| Floatable Float
-}
floatable : Float -> Expression
floatable val =
    Floatable val


{-| Negation (Node Expression)
-}
negation : Expression -> Expression
negation expr =
    Negation (nodify expr)


{-| Literal String
-}
literal : String -> Expression
literal val =
    Literal val


{-| CharLiteral Char
-}
charLiteral : Char -> Expression
charLiteral val =
    CharLiteral val


{-| TupledExpression (List (Node Expression))
-}
tupledExpression : List Expression -> Expression
tupledExpression exprs =
    TupledExpression (nodifyAll exprs)


{-| ParenthesizedExpression (Node Expression)
-}
parenthesizedExpression : Expression -> Expression
parenthesizedExpression expr =
    ParenthesizedExpression (nodify expr)


{-| LetExpression LetBlock
-}
letExpression : LetBlock -> Expression
letExpression letBlk =
    LetExpression letBlk


{-| CaseExpression CaseBlock
-}
caseExpression : CaseBlock -> Expression
caseExpression caseBlk =
    CaseExpression caseBlk


{-| LambdaExpression Lambda
-}
lambdaExpression : Lambda -> Expression
lambdaExpression lambda =
    LambdaExpression lambda


{-| RecordExpr (List (Node RecordSetter))
-}
recordExpr : List RecordSetter -> Expression
recordExpr setters =
    RecordExpr (nodifyAll setters)


{-| ListExpr (List (Node Expression))
-}
listExpr : List Expression -> Expression
listExpr exprs =
    ListExpr (nodifyAll exprs)


{-| RecordAccess (Node Expression) (Node String)
-}
recordAccess : Expression -> String -> Expression
recordAccess expr selector =
    RecordAccess (nodify expr) (nodify selector)


{-| RecordAccessFunction String
-}
recordAccessFunction : String -> Expression
recordAccessFunction selector =
    RecordAccessFunction selector


{-| RecordUpdateExpression (Node String) (List (Node RecordSetter))
-}
recordUpdateExpression : String -> List RecordSetter -> Expression
recordUpdateExpression varName setters =
    RecordUpdateExpression (nodify varName) (nodifyAll setters)


{-| GLSLExpression String
-}
glslExpression : String -> Expression
glslExpression expr =
    GLSLExpression expr



-- InfixDirection


left : InfixDirection
left =
    Left


right : InfixDirection
right =
    Right


non : InfixDirection
non =
    Non



-- LetBlock


letBlock : List LetDeclaration -> Expression -> LetBlock
letBlock decls expr =
    { declarations = nodifyAll decls
    , expression = nodify expr
    }


letFunction : Function -> LetDeclaration
letFunction func =
    letFunction func


letDestructuring : Pattern -> Expression -> LetDeclaration
letDestructuring pattern expr =
    LetDestructuring (nodify pattern) (nodify expr)



-- CaseBlock


caseBlock : Expression -> List Case -> CaseBlock
caseBlock expr cases =
    { expression = nodify expr
    , cases = cases
    }


type alias Cases =
    List Case


case_ : Pattern -> Expression -> Case
case_ pattern expr =
    ( nodify pattern, nodify expr )



-- Lambda
-- RecordSetter
--== Helpers


nodify : a -> Node a
nodify exp =
    Node emptyRange exp


nodifyMaybe : Maybe a -> Maybe (Node a)
nodifyMaybe =
    Maybe.map nodify


nodifyAll : List a -> List (Node a)
nodifyAll =
    List.map nodify
