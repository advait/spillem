module Stdlib exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


lib : Dict SpSymbol SpExpression
lib =
    Dict.fromList
        [ ( "+", BuiltinFun (numNumNum (+)) )
        , ( "-", BuiltinFun (numNumNum (-)) )
        , ( "*", BuiltinFun (numNumNum (*)) )
        , ( "/", BuiltinFun (numNumNum (//)) )
        , ( "%", BuiltinFun (numNumNum (modBy |> flip)) )
        , ( ">", BuiltinFun (numNumBool (>)) )
        , ( ">=", BuiltinFun (numNumBool (>=)) )
        , ( "<", BuiltinFun (numNumBool (<)) )
        , ( "<=", BuiltinFun (numNumBool (<=)) )
        ]


{-| Convert an elm function (Int -> Int -> Int) into a BuiltinFun with appropriate argument
error handling.
-}
numNumNum : (Int -> Int -> Int) -> (List SpExpression -> SpState -> SpState)
numNumNum f args state =
    case args of
        [ SpInt x, SpInt y ] ->
            { state | result = Ok <| SpInt (f x y) }

        _ ->
            { state | result = Err "RuntimeError" }


{-| Convert an elm function (Int -> Int -> Bool) into a BuiltinFun with appropriate argument
error handling.
-}
numNumBool : (Int -> Int -> Bool) -> (List SpExpression -> SpState -> SpState)
numNumBool f args state =
    let
        boolToExpr b =
            if b then
                SpSymbol "true"

            else
                SpSymbol "false"
    in
    case args of
        [ SpInt x, SpInt y ] ->
            { state | result = Ok (f x y |> boolToExpr) }

        _ ->
            { state | result = Err "RuntimeError" }


{-| Flips the order of arguments in a function.
-}
flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b
