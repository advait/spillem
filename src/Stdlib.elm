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
        , ( "=", BuiltinFun equals )
        , ( ">", BuiltinFun (numNumBool (>)) )
        , ( ">=", BuiltinFun (numNumBool (>=)) )
        , ( "<", BuiltinFun (numNumBool (<)) )
        , ( "<=", BuiltinFun (numNumBool (<=)) )
        ]


{-| Spillem equality check. Ints and symbols are equal by value. Lists are recursively checked for equality. No type
casting so items of different types are always not equal.
-}
equals : List SpExpression -> SpState -> SpState
equals args state =
    let
        internalEq : SpExpression -> SpExpression -> Bool
        internalEq a b =
            case ( a, b ) of
                ( SpInt intA, SpInt intB ) ->
                    intA == intB

                ( SpList listA, SpList listB ) ->
                    case ( listA, listB ) of
                        ( headA :: tailA, headB :: tailB ) ->
                            internalEq headA headB && internalEq (SpList tailA) (SpList tailB)

                        ( [], [] ) ->
                            True

                        _ ->
                            False

                ( SpSymbol symA, SpSymbol symB ) ->
                    symA == symB

                _ ->
                    False
    in
    case args of
        [ a, b ] ->
            { state | result = Ok (internalEq a b |> boolToExpr) }

        _ ->
            { state | result = Err "Invalid number of arguments" }


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


{-| Converts an elm bool into the equivalent SpSymbol representation.
-}
boolToExpr b =
    if b then
        SpSymbol "true"

    else
        SpSymbol "false"
