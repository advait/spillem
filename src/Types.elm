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
    | BuiltinFun (Env -> List SpExpression -> EvalResult)


{-| An Environment is a mapping of Symbols to expressions.
-}
type alias Env =
    Dict SpSymbol SpExpression


{-| An EvalResult is the result of evaluating an expression. If successful it is another expression and a new
environment. If unsuccessful the result is a String describing the error.
-}
type alias EvalResult =
    { result : Result String SpExpression
    , env : Env
    }
