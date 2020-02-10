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
    | BuiltinFun (SpState -> List SpExpression -> SpState)
    | SpNothing


{-| An Environment is a mapping of Symbols to expressions.
-}
type alias Env =
    Dict SpSymbol SpExpression


{-| Represents the entire state of the interpreter. The result represents the result of evaluating
the last expression.
-}
type alias SpState =
    { result : Result String SpExpression
    , env : Env
    }
