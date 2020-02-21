module Eval exposing (..)

import Env
import Types exposing (..)


initState : SpState
initState =
    { env = Env.defaultEnv, result = Ok SpNothing }


{-| Evaluate an expression in the context of an environment, producing a result.
TODO(advait): Change argument order of eval to best support chaining.
-}
eval : SpExpression -> SpState -> SpState
eval expr state =
    case expr of
        -- Integers evaluate to themselves
        SpInt i ->
            { state | result = Ok <| SpInt i }

        -- Evaluating symbols dereferences the symbols in the environment unless it is a special reserved symbol
        SpSymbol s ->
            if List.member s [ "true", "false", "nil" ] then
                { state | result = Ok <| SpSymbol s }

            else
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
            eval value state
                |> evalAndThen
                    (\evaluatedValue newState ->
                        { result = Ok <| evaluatedValue
                        , env = newState.env |> Env.setGlobal key evaluatedValue
                        }
                    )

        -- Special form for let* bindings
        SpList ((SpSymbol "let*") :: (SpList allBindings) :: [ returnValue ]) ->
            let
                insertBinding : SpSymbol -> SpExpression -> SpState -> SpState
                insertBinding key value nextState =
                    { env = nextState.env |> Env.setLocal key value
                    , result = Ok <| value
                    }

                processBindings : List SpExpression -> SpState -> SpState
                processBindings bindings inState =
                    case bindings of
                        (SpSymbol symbol) :: value :: tail ->
                            eval value inState
                                |> evalAndThen (insertBinding symbol)
                                |> evalAndThen (\_ -> processBindings tail)

                        [] ->
                            inState

                        _ ->
                            { inState | result = Err "Incorrect let* syntax" }
            in
            state
                |> (\s -> { s | env = s.env |> Env.pushScope })
                |> processBindings allBindings
                |> evalIfNotError returnValue
                |> (\s -> { s | env = s.env |> Env.popScope })

        -- Special form for do
        SpList ((SpSymbol "do") :: exprs) ->
            exprs |> List.foldl evalIfNotError { state | result = Ok <| SpNothing }

        -- Special form for if with three arguments
        SpList [ SpSymbol "if", cond, ifTrue, ifFalse ] ->
            eval cond state
                |> evalAndThen
                    (\evaluatedCond nextState ->
                        if evaluatedCond == SpSymbol "false" || evaluatedCond == SpSymbol "nil" then
                            eval ifFalse nextState

                        else
                            eval ifTrue nextState
                    )

        -- Function calls first evaluate all of the items in the list and then call the function with the args
        SpList exprs ->
            let
                ( finalState, exprsResult ) =
                    evalList exprs state
            in
            case exprsResult of
                -- Failed to evaluate one of the arguments
                Err err ->
                    { finalState | result = Err err }

                -- Successfully evaluated all arguments. Now apply function with evaluated arguments.
                Ok (fun :: args) ->
                    apply fun args finalState

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
apply : SpExpression -> List SpExpression -> SpState -> SpState
apply fun args state =
    case fun of
        BuiltinFun builtinFun ->
            builtinFun state args

        _ ->
            { state
                | result = Err "Invalid function form"
            }


{-| Evaluate a list of expressions. If any of them fail, stop and provide the failure.
-}
evalList : List SpExpression -> SpState -> ( SpState, Result String (List SpExpression) )
evalList exprs state =
    let
        rec : List SpExpression -> List SpExpression -> SpState -> ( SpState, Result String (List SpExpression) )
        rec acc exprsRec stateRec =
            case exprsRec of
                [] ->
                    ( stateRec, Ok acc )

                head :: tail ->
                    let
                        evaluatedHead =
                            eval head stateRec
                    in
                    case evaluatedHead.result of
                        Err err ->
                            ( evaluatedHead, Err err )

                        Ok ok ->
                            rec (acc ++ [ ok ]) tail evaluatedHead
    in
    rec [] exprs state


{-| Evaluates all the expressions, appropriately piping the environment through, and returning the result
of the last expression or the result of the first failed expression.
-}
evalAll : List SpExpression -> SpState -> SpState
evalAll exprs state =
    let
        ( finalState, result ) =
            evalList exprs state

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
evalAndThen : (SpExpression -> SpState -> SpState) -> SpState -> SpState
evalAndThen deferred result =
    case result.result of
        Ok ok ->
            deferred ok result

        Err err ->
            { result = Err err, env = result.env }


{-| Evaluate the given expression only if the current state is not an error. Useful for chaining.
-}
evalIfNotError : SpExpression -> SpState -> SpState
evalIfNotError expr state =
    case state.result of
        Err _ ->
            state

        _ ->
            eval expr state
