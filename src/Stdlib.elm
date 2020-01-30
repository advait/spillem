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
num2 : (Int -> Int -> Int) -> (List SpExpression -> EvalResult)
num2 f args =
    case args of
        [ SpInt x, SpInt y ] ->
            Ok <| SpInt (f x y)

        _ ->
            Err "Invalid call to plus"
