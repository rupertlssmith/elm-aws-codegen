module L1 exposing
    ( Basic(..)
    , Container(..)
    , Declarable(..)
    , Declarations
    , Restricted(..)
    , Type(..)
    )

{-| L1 is a data modelling language.

The name L1 stands for 'level 1', as it is envisioned to be part of a sequence of
similar data modelling languages, 'level 2', 'level 3'. With L1 being completely
language agnostic, and higher levels bringing in more specific information targetting
code generation on particular languages or systems.

What you see here is a first pass at describing the L1 modelling language, and using
it in a practical context of generating AWS Service stubs for Elm.

-}

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
    | RString { minLength : Maybe Int, maxLength : Maybe Int, regex : Maybe String }


type Declarable
    = DAlias Type
    | DSum (List ( String, List ( String, Type ) ))
    | DEnum (List String)
    | DRestricted Restricted


type alias Declarations =
    Dict String Declarable
