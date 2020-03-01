module Eval exposing (..)

import Core
import Dict
import Env
import Stdlib
import Types exposing (..)


{-| The default vanilla environment.
-}
initState : SpState
initState =
    let
        baseEnv =
            Env
                { bindings = Stdlib.lib
                , parentScope = Nothing
                }

        baseState =
            { env = baseEnv, result = Ok SpNothing }

        evaluatedCore =
            evalAll Core.core baseState
    in
    case evaluatedCore.result of
        Ok _ ->
            { evaluatedCore | result = Ok SpNothing }

        Err err ->
            Debug.todo err


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
        SpSymbol symbol ->
            if List.member expr [ spTrue, spFalse, spNil ] then
                { state | result = Ok expr }

            else
                Env.stateLookupSymbol symbol state

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
        SpList ((SpSymbol "let*") :: (SpList bindings) :: [ returnValue ]) ->
            let
                evalAndInsertBinding : SpSymbol -> SpExpression -> SpState -> SpState
                evalAndInsertBinding key unevaluatedBinding curState =
                    eval unevaluatedBinding curState
                        |> evalAndThen
                            (\value finalState ->
                                { env = finalState.env |> Env.setLocal key value
                                , result = Ok <| value
                                }
                            )
            in
            state
                |> Env.statePushScope
                |> reduceLetBindings evalAndInsertBinding bindings
                |> evalIfNotError returnValue
                |> Env.statePopScope

        -- Special form for letrec bindings
        -- Here, the values are dependent on the environment and the environment is dependent on the values
        -- (the "rec" in "letrec"). Elm nicely supports recursively defined values for this use case.
        SpList ((SpSymbol "letrec") :: (SpList bindings) :: [ returnValue ]) ->
            let
                -- fullyBoundState consists of the environment after all of the bindings are set.
                -- Because of the recursive definition, we must introduce a (() -> SpState) here instead of a
                -- direct SpState.
                -- See: https://github.com/elm/compiler/blob/master/hints/bad-recursion.md
                fullyBoundState : () -> SpState
                fullyBoundState _ =
                    state
                        |> Env.statePushScope
                        |> reduceLetBindings createLazyBinding bindings

                createLazyBinding : SpSymbol -> SpExpression -> SpState -> SpState
                createLazyBinding key unevaluatedValue inState =
                    let
                        lazyEvaluateValue =
                            -- TODO(advait): There is a bug here. Note how we are discarding the environment
                            -- and only paying attention to the returned result. If this lazy expression modifies
                            -- the environment with def!, etc., we will lose those modifications.
                            \_ -> eval unevaluatedValue (fullyBoundState ()) |> .result
                    in
                    { inState | env = inState.env |> Env.setLazyLocal key lazyEvaluateValue }
            in
            fullyBoundState ()
                |> evalIfNotError returnValue
                |> Env.statePopScope

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

        -- Special form for quote
        SpList [ SpSymbol "quote", quoted ] ->
            { state | result = Ok quoted }

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
                            inState
                                |> insertBinding name value
                                |> processBindings namesTail valuesTail

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


{-| Given a list of alternating Symbol + Expressions, reduce the list with the reducer function. If the reducer
returns an error at any point, halt and return that error.
-}
reduceLetBindings : (SpSymbol -> SpExpression -> SpState -> SpState) -> List SpExpression -> SpState -> SpState
reduceLetBindings reducer bindings state =
    case bindings of
        (SpSymbol symbol) :: value :: tail ->
            reducer symbol value state
                |> evalAndThen (\_ -> reduceLetBindings reducer tail)

        [] ->
            state

        _ :: _ :: _ ->
            { state | result = Err "Expected symbol" }

        _ ->
            { state | result = Err "Not enough arguments in bindings" }


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
                    finalState

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
