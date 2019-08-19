module ElmDSL exposing (..)

--import Elm.Syntax.Comments exposing (Comment)
--import Elm.Syntax.Documentation exposing (Documentation)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (ExposedType, Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (Infix, InfixDirection(..))
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Location, Range, emptyRange)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))



--== Re-Export of Types


type alias Comment =
    String


type alias Documentation =
    String


type alias ModuleName =
    List String


type alias Module =
    Elm.Syntax.Module.Module


type alias File =
    Elm.Syntax.File.File


type alias Declaration =
    Elm.Syntax.Declaration.Declaration


type alias Import =
    Elm.Syntax.Import.Import


type alias TypeAnnotation =
    Elm.Syntax.TypeAnnotation.TypeAnnotation


type alias Exposing =
    Elm.Syntax.Exposing.Exposing


type alias TopLevelExpose =
    Elm.Syntax.Exposing.TopLevelExpose


type alias Expression =
    Elm.Syntax.Expression.Expression


type alias InfixDirection =
    Elm.Syntax.Infix.InfixDirection


type alias Pattern =
    Elm.Syntax.Pattern.Pattern



--== Elm.Syntax.Declaration


{-| FunctionDeclaration Function
-}
functionDeclaration : Maybe Documentation -> Maybe Signature -> String -> List Pattern -> Expression -> Declaration
functionDeclaration docs sig name args expr =
    functionImplementation name args expr
        |> function docs sig
        |> FunctionDeclaration


{-| AliasDeclaration TypeAlias
-}
aliasDeclaration : TypeAlias -> Declaration
aliasDeclaration tAlias =
    AliasDeclaration tAlias


{-| CustomTypeDeclaration Type
-}
customTypeDeclaration : Type -> Declaration
customTypeDeclaration typ =
    CustomTypeDeclaration typ


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
    InfixExpose sym


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


openExposedType : String -> ExposedType
openExposedType name =
    { name = name
    , open = Just emptyRange
    }


closedExposedType : String -> ExposedType
closedExposedType name =
    { name = name
    , open = Nothing
    }



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
lambdaExpression lambd =
    LambdaExpression lambd


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


lambda : List Pattern -> Expression -> Lambda
lambda args expr =
    { args = nodifyAll args
    , expression = nodify expr
    }


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


recordSetter : String -> Expression -> RecordSetter
recordSetter field expr =
    ( nodify field, nodify expr )


caseBlock : Expression -> List Case -> CaseBlock
caseBlock expr cases =
    { expression = nodify expr
    , cases = cases
    }


case_ : Pattern -> Expression -> Case
case_ pattern expr =
    ( nodify pattern, nodify expr )


function : Maybe Documentation -> Maybe Signature -> FunctionImplementation -> Function
function docs sig decl =
    { documentation = nodifyMaybe docs
    , signature = nodifyMaybe sig
    , declaration = nodify decl
    }


functionImplementation : String -> List Pattern -> Expression -> FunctionImplementation
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
    NormalModule <| defaultModuleData name (exposing_ exposes)


{-| PortModule DefaultModuleData
-}
portModule : ModuleName -> List TopLevelExpose -> Module
portModule name exposes =
    PortModule <| defaultModuleData name (exposing_ exposes)


{-| EffectModule EffectModuleData
-}
effectModule : ModuleName -> List TopLevelExpose -> Maybe String -> Maybe String -> Module
effectModule name exposes cmd sub =
    EffectModule <| effectModuleData name (exposing_ exposes) cmd sub


defaultModuleData : ModuleName -> Exposing -> DefaultModuleData
defaultModuleData name exposes =
    { moduleName = nodify name
    , exposingList = nodify exposes
    }


effectModuleData : ModuleName -> Exposing -> Maybe String -> Maybe String -> EffectModuleData
effectModuleData name exposes cmd sub =
    { moduleName = nodify name
    , exposingList = nodify exposes
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
floatPattern : Float -> Pattern
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



--== Elm.Syntax.Type


type_ : Maybe Documentation -> String -> List String -> List ValueConstructor -> Type
type_ docs name args constructors =
    { documentation = nodifyMaybe docs
    , name = nodify name
    , generics = nodifyAll args
    , constructors = nodifyAll constructors
    }


valueConstructor : String -> List TypeAnnotation -> ValueConstructor
valueConstructor name annotations =
    { name = nodify name
    , arguments = nodifyAll annotations
    }



--== Elm.Syntax.TypeAlias


typeAlias : Maybe Documentation -> String -> List String -> TypeAnnotation -> TypeAlias
typeAlias docs name args annotation =
    { documentation = nodifyMaybe docs
    , name = nodify name
    , generics = nodifyAll args
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


{-| Creates an `exposing` section for a module or an import. If the list is empty
`exposing (..)` will be created, otherwise an explicit list of `TopLevelExpose`s
is created.
-}
exposing_ : List TopLevelExpose -> Exposing
exposing_ exposes =
    case exposes of
        [] ->
            All emptyRange

        es ->
            Explicit <| nodifyAll exposes
