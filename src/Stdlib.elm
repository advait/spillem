module Stdlib exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


lib : Dict SpSymbol SpExpression
lib =
    Dict.fromList
        [ ( "+", BuiltinFun plus )
        ]


plus : List SpExpression -> EvalResult
plus args =
    case args of
        [ SpInt x, SpInt y ] ->
            Ok <| SpInt (x + y)

        _ ->
            Err "Invalid call to plus"
