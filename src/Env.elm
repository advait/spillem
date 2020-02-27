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


createIndirectRef : SpSymbol -> Env -> Env
createIndirectRef key (Env env) =
    Env { env | bindings = Dict.insert key IndirectRef env.bindings }


setIndirectRef : SpSymbol -> SpExpression -> Env -> Env
setIndirectRef key value (Env env) =
    Env { env | indirectBindings = Dict.insert key value env.indirectBindings }


{-| Recursively looks up a symbol in each of the scope, preferring inner scopes.
-}
lookupSymbol : SpSymbol -> Env -> Maybe SpExpression
lookupSymbol key (Env env) =
    let
        lookupIndirect : Env -> Maybe SpExpression
        lookupIndirect (Env inEnv) =
            case inEnv.indirectBindings |> Dict.get key of
                Just exp ->
                    Just exp

                Nothing ->
                    case inEnv.parentScope of
                        Nothing ->
                            Nothing

                        Just parent ->
                            lookupIndirect parent

        lookupDirect : Env -> Maybe SpExpression
        lookupDirect (Env inEnv) =
            case inEnv.bindings |> Dict.get key of
                Just (DirectRef exp) ->
                    Just exp

                Just IndirectRef ->
                    lookupIndirect (Env env)

                Nothing ->
                    case inEnv.parentScope of
                        Nothing ->
                            Nothing

                        Just parent ->
                            lookupDirect parent
    in
    lookupDirect (Env env)


{-| Pushes a scope onto the scope stack.
-}
pushScope : Env -> Env
pushScope env =
    Env { bindings = Dict.empty, indirectBindings = Dict.empty, parentScope = Just env }


{-| Pops a scope from the scope stack.
-}
popScope : Env -> Env
popScope (Env env) =
    case env.parentScope of
        Nothing ->
            Debug.todo "Cannot pop global scope"

        Just parent ->
            parent
