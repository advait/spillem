module Eval exposing (..)

import Dict exposing (Dict)
import Env
import Stdlib
import Types exposing (..)


initState : SpState
initState =
    { env = Env.defaultEnv, result = Ok SpNothing }


{-| Evaluate an expression in the context of an environment, producing a result.
-}
eval : SpState -> SpExpression -> SpState
eval state expr =
    case expr of
        -- Integers evaluate to themselves
        SpInt i ->
            { state | result = Ok <| SpInt i }

        -- Evaluating symbols dereferences the symbols in the environment
        SpSymbol s ->
            { state
                | result =
                    state.env
                        |> Env.lookupSymbol s
                        |> Result.fromMaybe ("ReferenceError: " ++ s)
            }

        -- Empty list evaluates to itself
        SpList [] ->
            { state | result = Ok <| SpList [] }

        -- Special form for def!
        SpList [ SpSymbol "def!", SpSymbol key, value ] ->
            evalAndThen (eval state value)
                (\newEnv evaluatedValue ->
                    { result = Ok <| evaluatedValue
                    , env = newEnv |> Env.setGlobal key evaluatedValue
                    }
                )

        -- Function calls first evaluate all of the items in the list and then call the function with the args
        SpList exprs ->
            let
                ( finalState, exprsResult ) =
                    evalList state exprs
            in
            case exprsResult of
                -- Failed to evaluate one of the arguments
                Err err ->
                    { finalState | result = Err err }

                -- Successfully evaluated all arguments. Now apply function with evaluated arguments.
                Ok (fun :: args) ->
                    apply finalState fun args

                Ok [] ->
                    Debug.todo "Will never happen. Handled by 'SpList []' case above."

        -- Builtins evaluate to themselves
        BuiltinFun f ->
            { state | result = Ok <| BuiltinFun f }

        -- SpNothing evaluates to itself
        SpNothing ->
            { state | result = Ok <| SpNothing }


{-| Call a function with the given arguments.
-}
apply : SpState -> SpExpression -> List SpExpression -> SpState
apply state fun args =
    case fun of
        BuiltinFun builtinFun ->
            builtinFun state args

        _ ->
            { state
                | result = Err "Invalid function form"
            }


{-| Evaluate a list of expressions. If any of them fail, stop and provide the failure.
-}
evalList : SpState -> List SpExpression -> ( SpState, Result String (List SpExpression) )
evalList state exprs =
    let
        rec : List SpExpression -> SpState -> List SpExpression -> ( SpState, Result String (List SpExpression) )
        rec acc stateRec exprsRec =
            case exprsRec of
                [] ->
                    ( stateRec, Ok acc )

                head :: tail ->
                    let
                        evaluatedHead =
                            eval stateRec head
                    in
                    case evaluatedHead.result of
                        Err err ->
                            ( evaluatedHead, Err err )

                        Ok ok ->
                            rec (acc ++ [ ok ]) evaluatedHead tail
    in
    rec [] state exprs


{-| Evaluates all the expressions, appropriately piping the environment through, and returning the result
of the last expression or the result of the first failed expression.
-}
evalAll : SpState -> List SpExpression -> SpState
evalAll state exprs =
    let
        ( finalState, result ) =
            evalList state exprs

        lastElem : Result String (List SpExpression) -> SpState
        lastElem elems =
            case elems of
                Err err ->
                    { finalState | result = Err err }

                Ok [] ->
                    { finalState | result = Err "Nothing to evaluate." }

                Ok [ final ] ->
                    { finalState | result = Ok final }

                Ok (_ :: tail) ->
                    lastElem (Ok tail)
    in
    lastElem result


{-| If the provided result failed, simply provide the failed result. If it succeeded, pipe the new environment to
evaluate the second expression, returning the result.
-}
evalAndThen : SpState -> (Env -> SpExpression -> SpState) -> SpState
evalAndThen result deferred =
    case result.result of
        Ok ok ->
            deferred result.env ok

        Err err ->
            { result = Err err, env = result.env }
