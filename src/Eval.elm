module Eval exposing (..)

import Dict exposing (Dict)
import Stdlib
import Types exposing (..)


defaultEnv : Env
defaultEnv =
    Stdlib.lib


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

        -- Special form for def!
        SpList [ SpSymbol "def!", SpSymbol key, value ] ->
            evalAndThen (eval env value)
                (\newEnv evaluatedValue ->
                    { result = Ok <| evaluatedValue
                    , env = newEnv |> Dict.insert key evaluatedValue
                    }
                )

        -- Function calls first evaluate all of the items in the list and then call the function with the args
        SpList exprs ->
            let
                ( newEnv, exprsResult ) =
                    evalList env exprs
            in
            case exprsResult of
                Err err ->
                    { result = Err err
                    , env = newEnv
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

        -- SpNothing evaluates to itself
        SpNothing ->
            { result = Ok <| SpNothing
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
evalList : Env -> List SpExpression -> ( Env, Result String (List SpExpression) )
evalList env exprs =
    let
        rec acc envRec exprsRec =
            case exprsRec of
                [] ->
                    ( envRec, Ok acc )

                head :: tail ->
                    let
                        evaluatedHead =
                            eval envRec head
                    in
                    case evaluatedHead.result of
                        Err err ->
                            ( evaluatedHead.env, Err err )

                        Ok ok ->
                            rec (acc ++ [ ok ]) evaluatedHead.env tail
    in
    rec [] env exprs


{-| Evaluates all the expressions, appropriately piping the environment through, and returning the result
of the last expression or the result of the first failed expression.
-}
evalAll : Env -> List SpExpression -> EvalResult
evalAll env exprs =
    let
        ( finalEnv, result ) =
            evalList env exprs

        lastElem : Result String (List SpExpression) -> EvalResult
        lastElem elems =
            case elems of
                Err err ->
                    { env = finalEnv, result = Err err }

                Ok [] ->
                    { env = finalEnv, result = Err "Nothing to evaluate." }

                Ok [ final ] ->
                    { env = finalEnv, result = Ok final }

                Ok (_ :: tail) ->
                    lastElem (Ok tail)
    in
    lastElem result


{-| If the provided result failed, simply provide the failed result. If it succeeded, pipe the new environment to
evaluate the second expression, returning the result.
-}
evalAndThen : EvalResult -> (Env -> SpExpression -> EvalResult) -> EvalResult
evalAndThen result deferred =
    case result.result of
        Ok ok ->
            deferred result.env ok

        Err err ->
            { result = Err err, env = result.env }
