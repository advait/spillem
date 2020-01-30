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
            { result = Ok <| SpInt i
            , env = env
            }

        -- Evaluating symbols dereferences the symbols in the environment
        SpSymbol s ->
            { result =
                env
                    |> Dict.get s
                    |> Result.fromMaybe ("ReferenceError: " ++ s)
            , env = env
            }

        -- Empty list evaluates to itself
        SpList [] ->
            { result = Ok <| SpList []
            , env = env
            }

        -- Function calls first evaluate all of the items in the list and then call the function with the args
        SpList exprs ->
            case evalArgs env exprs of
                Err err ->
                    { result = Err err
                    , env = env
                    }

                Ok (fun :: args) ->
                    apply env fun args

                Ok [] ->
                    Debug.todo "Will never happen. Handled by 'SpList []' case above."

        -- Builtins evaluate to themselves
        BuiltinFun f ->
            { result = Ok <| BuiltinFun f
            , env = env
            }


{-| Call a function with the given arguments.
-}
apply : Env -> SpExpression -> List SpExpression -> EvalResult
apply env fun args =
    case fun of
        BuiltinFun builtinFun ->
            builtinFun env args

        _ ->
            { result = Err "Invalid function form"
            , env = env
            }


{-| Evaluate a list of expressions. If any of them fail, stop and provide the failure.
-}
evalArgs : Env -> List SpExpression -> Result String (List SpExpression)
evalArgs env exprs =
    let
        -- TODO(advait): Args might actually modify environment! We need to pipe this through each arg eval.
        evaluatedExprs =
            exprs |> List.map (eval env) |> List.map .result
    in
    evaluatedExprs |> List.foldr (Result.map2 (::)) (Ok [])
