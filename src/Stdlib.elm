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
num2 : (Int -> Int -> Int) -> (Env -> List SpExpression -> EvalResult)
num2 f env args =
    case args of
        [ SpInt x, SpInt y ] ->
            { result = Ok <| SpInt (f x y)
            , env = env
            }

        _ ->
            { result = Err "RuntimeError"
            , env = env
            }
