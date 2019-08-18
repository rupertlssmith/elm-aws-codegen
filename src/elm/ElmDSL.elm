module ElmDSL exposing
    ( Cases
    , application
    , caseBlock
    , caseExpression
    , case_
    , charLiteral
    , file
    , floatable
    , functionDeclaration
    , functionOrValue
    , functionTypeAnnotation
    , genericRecord
    , genericType
    , glslExpression
    , hex
    , ifBlock
    , import_
    , integer
    , lambdaExpression
    , left
    , letBlock
    , letDestructuring
    , letExpression
    , letFunction
    , listExpr
    , literal
    , negation
    , nodify
    , nodifyAll
    , nodifyMaybe
    , non
    , normalModule
    , operator
    , operatorApplication
    , parenthesizedExpression
    , prefixOperator
    , record
    , recordAccess
    , recordAccessFunction
    , recordExpr
    , recordUpdateExpression
    , right
    , signature
    , tupled
    , tupledExpression
    , typed
    , unit
    , unitExpr
    )

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


type alias Cases =
    List Case


type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }



--== Elm.Syntax.Declaration


{-| FunctionDeclaration Function
-}
functionDeclaration : Function -> Declaration
functionDeclaration fn =
    FunctionDeclaration fn


{-| AliasDeclaration TypeAlias
-}
aliasDeclaration : TypeAlias -> Declaration
aliasDeclaration typeAlias =
    AliasDeclaration typeAlias


{-| CustomTypeDeclaration Type
-}
customTypeDeclaration : Type -> Declaration
customTypeDeclaration type_ =
    CustomTypeDeclaration type_


{-| PortDeclaration Signature
-}
portDeclaration : Signature -> Declaration
portDeclaration sig =
    PortDeclaration sig


{-| InfixDeclaration Infix
-}
infixDeclaration : Infix -> Declaration
infixDeclaration inf =
    InfixDeclaration inf


{-| Destructuring (Node Pattern) (Node Expression)
-}
destructuring : Pattern -> Expression -> Declaration
destructuring pattern expr =
    Destructuring (nodify pattern) (nodify expr)



--== Elm.Syntax.Exposing


{-| All Range
-}
all : Range -> Exposing
all range =
    All range


{-| Explicit (List (Node TopLevelExpose))
-}
explicit : List TopLevelExpose -> Exposing
explicit topLevelExposes =
    Explicit (nodifyAll topLevelExposes)


{-| InfixExpose String
-}
infixExpose : String -> TopLevelExpose
infixExpose sym =
    InfixExpose (nodify sym)


{-| FunctionExpose String
-}
functionExpose : String -> TopLevelExpose
functionExpose fn =
    FunctionExpose fn


{-| TypeOrAliasExpose String
-}
typeOrAliasExpose : String -> TopLevelExpose
typeOrAliasExpose name =
    TypeOrAliasExpose name


{-| TypeExpose ExposedType
-}
typeExpose : ExposedType -> TopLevelExpose
typeExpose exposedType =
    TypeExpose exposedType



--== Elm.Syntax.Expression


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



-- Lambda


lambda : List Pattern -> Expression -> Lambda
lambda args expr =
    { args = nodifyAll args
    , expression = nodify expr
    }



-- LetBlock


letBlock : List LetDeclaration -> Expression -> LetBlock
letBlock decls expr =
    { declarations = nodifyAll decls
    , expression = nodify expr
    }


{-| LetFunction Function
-}
letFunction : Function -> LetDeclaration
letFunction func =
    LetFunction func


{-| LetDestructuring (Node Pattern) (Node Expression)
-}
letDestructuring : Pattern -> Expression -> LetDeclaration
letDestructuring pattern expr =
    LetDestructuring (nodify pattern) (nodify expr)



-- RecordSetter


recordSetter : String -> Expression -> RecordSetter
recordSetter field expr =
    ( nodify field, nodify expr )



-- CaseBlock


caseBlock : Expression -> List Case -> CaseBlock
caseBlock expr cases =
    { expression = nodify expr
    , cases = cases
    }


case_ : Pattern -> Expression -> Case
case_ pattern expr =
    ( nodify pattern, nodify expr )


function : Documentation -> Signature -> FunctionImplementation
function docs sig decl =
    { documentation = nodifyMaybe docs
    , signature = nodifyMaybe sig
    , declaration = nodify decl
    }


functionImplementation : String -> List Pattern -> Expression
functionImplementation name args expr =
    { name = nodify name
    , arguments = nodifyAll args
    , expression = nodify expr
    }



--== Elm.Syntax.File


file : Module -> List Import -> List Declaration -> List Comment -> File
file mod imports declarations comments =
    { moduleDefinition = nodify mod
    , imports = nodifyAll imports
    , declarations = nodifyAll declarations
    , comments = nodifyAll comments
    }



--== Elm.Syntax.Import


import_ : ModuleName -> Maybe ModuleName -> Maybe Exposing -> Import
import_ modName aliasName exposes =
    { moduleName = nodify modName
    , moduleAlias = nodifyMaybe aliasName
    , exposingList = nodifyMaybe exposes
    }



--== Elm.Syntax.Infix


infix_ : InfixDirection -> Int -> String -> String -> Infix
infix_ direction precedence symbol fn =
    { direction = nodify direction
    , precedence = nodify precedence
    , operator = nodify symbol
    , function = nodify fn
    }


{-| Left
-}
left : InfixDirection
left =
    Left


{-| Right
-}
right : InfixDirection
right =
    Right


{-| Non
-}
non : InfixDirection
non =
    Non



--== Elm.Syntax.Module


{-| NormalModule DefaultModuleData
-}
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


{-| PortModule DefaultModuleData
-}
portModule : ModuleName -> List TopLevelExpose -> Module
portModule name exposes =
    let
        exposingList =
            case exposes of
                [] ->
                    All emptyRange

                es ->
                    Explicit <| nodifyAll exposes
    in
    PortModule
        { moduleName = nodify name
        , exposingList = nodify exposingList
        }


{-| EffectModule EffectModuleData
-}
effectModule : ModuleName -> List TopLevelExpose -> Maybe String -> Maybe String -> Module
effectModule name exposes cmd sub =
    let
        exposingList =
            case exposes of
                [] ->
                    All emptyRange

                es ->
                    Explicit <| nodifyAll exposes
    in
    PortModule
        { moduleName = nodify name
        , exposingList = nodify exposingList
        , command = nodifyMaybe cmd
        , subscription = nodifyMaybe sub
        }



--== Elm.Syntax.Pattern


{-| AllPattern
-}
allPattern : Pattern
allPattern =
    AllPattern


{-| UnitPattern
-}
unitPattern : Pattern
unitPattern =
    UnitPattern


{-| CharPattern Char
-}
charPattern : Char -> Pattern
charPattern char =
    CharPattern char


{-| StringPattern String
-}
stringPattern : String -> Pattern
stringPattern val =
    StringPattern val


{-| IntPattern Int
-}
intPattern : Int -> Pattern
intPattern val =
    IntPattern val


{-| HexPattern Int
-}
hexPattern : Int -> Pattern
hexPattern val =
    HexPattern val


{-| FloatPattern Float
-}
floatPattern : float -> Pattern
floatPattern val =
    FloatPattern val


{-| TuplePattern (List (Node Pattern))
-}
tuplePattern : List Pattern -> Pattern
tuplePattern patterns =
    TuplePattern (nodifyAll patterns)


{-| RecordPattern (List (Node String))
-}
recordPattern : List String -> Pattern
recordPattern fields =
    RecordPattern (nodifyAll fields)


{-| UnConsPattern (Node Pattern) (Node Pattern)
-}
unConsPattern : Pattern -> Pattern -> Pattern
unConsPattern hd tl =
    UnConsPattern (nodify hd) (nodify tl)


{-| ListPattern (List (Node Pattern))
-}
listPattern : List Pattern -> Pattern
listPattern seq =
    ListPattern (nodifyAll seq)


{-| VarPattern String
-}
varPattern : String -> Pattern
varPattern name =
    VarPattern name


{-| NamedPattern QualifiedNameRef (List (Node Pattern))
-}
namedPattern : QualifiedNameRef -> List Pattern -> Pattern
namedPattern qualName patterns =
    NamedPattern qualName (nodifyAll patterns)


{-| AsPattern (Node Pattern) (Node String)
-}
asPattern : Pattern -> String -> Pattern
asPattern pattern name =
    AsPattern (nodify pattern) (nodify name)


{-| ParenthesizedPattern (Node Pattern)
-}
paranthesizedPattern : Pattern -> Pattern
paranthesizedPattern pattern =
    ParenthesizedPattern (nodify pattern)



--== Elm.Syntax.Signature


signature : String -> TypeAnnotation -> Signature
signature name annotation =
    { name = nodify name
    , typeAnnotation = nodify annotation
    }



--== Elm.Syntax.TypeAnnotation


{-| GenericType String
-}
genericType : String -> TypeAnnotation
genericType name =
    GenericType name


{-| Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
-}
typed : ModuleName -> String -> List TypeAnnotation -> TypeAnnotation
typed moduleName name args =
    Typed (nodify ( moduleName, name )) (nodifyAll args)


{-| Unit
-}
unit : TypeAnnotation
unit =
    Unit


{-| Tupled (List (Node TypeAnnotation))
-}
tupled : List TypeAnnotation -> TypeAnnotation
tupled args =
    Tupled (nodifyAll args)


{-| Record RecordDefinition
-}
record : RecordDefinition -> TypeAnnotation
record recDef =
    Record recDef


{-| GenericRecord (Node String) (Node RecordDefinition)
-}
genericRecord : String -> RecordDefinition -> TypeAnnotation
genericRecord argName recDef =
    GenericRecord (nodify argName) (nodify recDef)


{-| FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)
-}
functionTypeAnnotation : TypeAnnotation -> TypeAnnotation -> TypeAnnotation
functionTypeAnnotation arg result =
    FunctionTypeAnnotation (nodify arg) (nodify result)


{-| RecordDefinition
-}
recordDefinition : List RecordField -> RecordDefinition
recordDefinition fields =
    nodifyAll fields


{-| RecordField
-}
recordField : String -> TypeAnnotation -> RecordField
recordField field typeAnnotation =
    ( nodify field, nodify typeAnnotation )



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
