module Eval exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


{-| Evaluate an expression in the context of an environment, producing a result.
-}
eval : Env -> SpExpression -> EvalResult
eval env expr =
    case expr of
        -- Integers evaluate to themselves
        SpInt i ->
            Ok <| SpInt i

        -- Evaluating symbols dereferences the symbols in the environment
        SpSymbol s ->
            env
                |> Dict.get s
                |> Result.fromMaybe ("ReferenceError: " ++ s)

        -- Empty list evaluates to itself
        SpList [] ->
            Ok <| SpList []

        SpList exprs ->
            case evalArgs env exprs of
                Err err ->
                    Err err

                Ok (fun :: args) ->
                    apply env fun args

                Ok [] ->
                    Debug.todo "Will never happen. Handled by 'SpList []' case above."

        BuiltinFun f ->
            Ok <| BuiltinFun f


{-| Call a function with the given arguments.
-}
apply : Env -> SpExpression -> List SpExpression -> EvalResult
apply env fun args =
    case fun of
        BuiltinFun builtinFun ->
            builtinFun args

        _ ->
            Err "Invalid function form"


{-| Evaluate a list of expressions. If any of them fail, stop and provide the failure.
-}
evalArgs : Env -> List SpExpression -> Result String (List SpExpression)
evalArgs env exprs =
    let
        evaluatedExprs =
            List.map (eval env) exprs
    in
    List.foldr (Result.map2 (::)) (Ok []) evaluatedExprs
