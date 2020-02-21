module Stdlib exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


lib : Dict SpSymbol SpExpression
lib =
    Dict.fromList
        [ ( "+", BuiltinFun (num2 (+)) )
        , ( "-", BuiltinFun (num2 (-)) )
        , ( "*", BuiltinFun (num2 (*)) )
        , ( "/", BuiltinFun (num2 (//)) )
        ]


{-| Convert an elm function that takes in two integer arguments into a BuiltinFun with appropriate argument
error handling.
-}
num2 : (Int -> Int -> Int) -> (List SpExpression -> SpState -> SpState)
num2 f args state =
    case args of
        [ SpInt x, SpInt y ] ->
            { state | result = Ok <| SpInt (f x y) }

        _ ->
            { state | result = Err "RuntimeError" }
