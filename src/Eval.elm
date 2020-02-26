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
            if List.member expr [ spTrue, spFalse, spNil ] then
                { state | result = Ok expr }

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

        -- Special form for fn* function closure definitions
        SpList [ SpSymbol "fn*", SpList args, body ] ->
            let
                processArgs : List SpExpression -> Result String (List SpSymbol)
                processArgs inArgs =
                    case inArgs of
                        (SpSymbol symbol) :: tail ->
                            Result.map2 (::) (Ok symbol) (processArgs tail)

                        [] ->
                            Ok []

                        _ ->
                            Err "Invalid function argument"
            in
            case processArgs args of
                Ok processedArgs ->
                    { state | result = Ok (ClosureFun state.env processedArgs body) }

                Err err ->
                    { state | result = Err err }

        -- Special form for do
        SpList ((SpSymbol "do") :: exprs) ->
            exprs |> List.foldl evalIfNotError { state | result = Ok <| SpNothing }

        -- Special form for if with three arguments
        SpList [ SpSymbol "if", cond, ifTrue, ifFalse ] ->
            eval cond state
                |> evalAndThen
                    (\evaluatedCond nextState ->
                        if evaluatedCond == spFalse || evaluatedCond == spNil then
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
        BuiltinFun _ ->
            { state | result = Ok <| expr }

        -- Function closures evaluate to themselves
        ClosureFun _ _ _ ->
            { state | result = Ok <| expr }

        -- SpNothing evaluates to itself
        SpNothing ->
            { state | result = Ok <| expr }


{-| Call a function with the given arguments.
-}
apply : SpExpression -> List SpExpression -> SpState -> SpState
apply fun callArgs state =
    case fun of
        BuiltinFun builtinFun ->
            builtinFun callArgs state

        ClosureFun closureEnv funArgs body ->
            let
                insertBinding : SpSymbol -> SpExpression -> SpState -> SpState
                insertBinding key value nextState =
                    { env = nextState.env |> Env.setLocal key value
                    , result = Ok <| value
                    }

                processBindings : List SpSymbol -> List SpExpression -> SpState -> SpState
                processBindings names values inState =
                    case ( names, values ) of
                        ( name :: namesTail, value :: valuesTail ) ->
                            eval value inState
                                |> evalAndThen (insertBinding name)
                                |> evalAndThen (\_ -> processBindings namesTail valuesTail)

                        ( [], [] ) ->
                            inState

                        _ ->
                            { inState | result = Err "Invalid number of arguments" }
            in
            { state | env = closureEnv }
                |> (\s -> { s | env = s.env |> Env.pushScope })
                |> processBindings funArgs callArgs
                |> evalIfNotError body
                -- Restore the original environment
                -- TODO(advait): There is a bug here. If the function body evaluation imperatively modifies the
                -- environment (def!), then we blow away those modifications when we restore the original env here.
                -- The ideal fix is to keep track of global/imperative state separately from closures.
                |> (\s -> { s | env = state.env })

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
