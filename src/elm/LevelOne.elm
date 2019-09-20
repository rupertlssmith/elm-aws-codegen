module LevelOne exposing (..)

import Dict exposing (Dict)


type Basic
    = BBool
    | BInt
    | BReal
    | BString


type Container
    = CList Type
    | CSet Type
    | CDict Type Type
    | COptional Type


type Type
    = TBasic Basic
    | TNamed String
    | TProduct (List ( String, Type ))
    | TContainer Container
    | TFunction Type Type


type Restricted
    = RInt { min : Maybe Int, max : Maybe Int, width : Maybe Int }
    | RString { minLength : Maybe Int, maxLength : Maybe Int, regex : String }


type Declarable
    = DAlias Type
    | DSum (List ( String, List ( String, Type ) ))
    | DRestricted Restricted


type alias Declarations =
    Dict String Declarable
