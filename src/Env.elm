module Env exposing (..)

import Dict exposing (Dict)
import Types exposing (..)


{-| Sets the given binding in the global (outer-most) scope.
-}
setGlobal : SpSymbol -> SpExpression -> Env -> Env
setGlobal key value env =
    let
        rec : Env -> Env
        rec (Env cur) =
            case cur.parentScope of
                Nothing ->
                    Env { cur | bindings = Dict.insert key (DirectRef value) cur.bindings }

                Just parent ->
                    Env { cur | parentScope = Just <| rec parent }
    in
    rec env


{-| Sets the given binding in the local (inner-most) scope.
-}
setLocal : SpSymbol -> SpExpression -> Env -> Env
setLocal key value (Env env) =
    Env { env | bindings = Dict.insert key (DirectRef value) env.bindings }


{-| Sets a lazy binding in the local (inner-most) scope. Lazy bindings are necessary in situations
like letrec where the bindings refer to the environment and the environment refers to
the bindings in a mutually recursive fashion. We must break the hard recursive cycle by manually
introducing laziness. Here, a lazy value is only computed when it needed at runtime. Because of the
immutable nature of Elm, lazy values are always re-computed on read.
-}
setLazyLocal : SpSymbol -> (() -> Result String SpExpression) -> Env -> Env
setLazyLocal key lazyValue (Env env) =
    Env { env | bindings = Dict.insert key (LazyRef lazyValue) env.bindings }


{-| Recursively looks up a symbol in each of the scopes, preferring inner scopes. The value is placed
in the returned state's result. If the symbol is not found, a ReferenceError is placed in the result.
-}
stateLookupSymbol : SpSymbol -> SpState -> SpState
stateLookupSymbol key state =
    let
        lookupDirect : Env -> SpState
        lookupDirect (Env env) =
            case env.bindings |> Dict.get key of
                Just (DirectRef exp) ->
                    { state | result = Ok exp }

                Just (LazyRef ref) ->
                    { state | result = ref () }

                Nothing ->
                    case env.parentScope of
                        Nothing ->
                            { state | result = Err ("ReferenceError: " ++ key) }

                        Just parent ->
                            lookupDirect parent
    in
    lookupDirect state.env


{-| Pushes a scope onto the scope stack.
-}
pushScope : Env -> Env
pushScope env =
    Env { bindings = Dict.empty, parentScope = Just env }


{-| Like pushScope but operates on an SpState.
-}
statePushScope : SpState -> SpState
statePushScope state =
    { state | env = state.env |> pushScope }


{-| Pops a scope from the scope stack.
-}
popScope : Env -> Env
popScope (Env env) =
    case env.parentScope of
        Nothing ->
            Debug.todo "Cannot pop global scope"

        Just parent ->
            parent


{-| Like popScope but operates on an SpState.
-}
statePopScope : SpState -> SpState
statePopScope state =
    { state | env = state.env |> popScope }
