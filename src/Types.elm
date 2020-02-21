module Types exposing (..)

import Dict exposing (Dict)


type alias SpSymbol =
    String


{-| Represents an expression in the Spillem language.
-}
type SpExpression
    = SpInt Int
    | SpSymbol SpSymbol
    | SpList (List SpExpression)
    | BuiltinFun (List SpExpression -> SpState -> SpState)
    | SpNothing


{-| An Env is a linked-list of scopes where a scope is a dict of variable bindings from SpSymbol
to SpExpression. The head of the linked list is the inner-most scope. The Env always has at least
one scope in it (the global scope).
-}
type Env
    = Env
        { bindings : Dict SpSymbol SpExpression
        , parentScope : Maybe Env
        }


{-| Represents the entire state of the interpreter. The result represents the result of evaluating
the last expression.
-}
type alias SpState =
    { result : Result String SpExpression
    , env : Env
    }
