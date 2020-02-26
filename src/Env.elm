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
                    Env { cur | bindings = Dict.insert key value cur.bindings }

                Just parent ->
                    Env { cur | parentScope = Just <| rec parent }
    in
    rec env


{-| Sets the given binding in the local (inner-most) scope.
-}
setLocal : SpSymbol -> SpExpression -> Env -> Env
setLocal key value (Env env) =
    Env { env | bindings = Dict.insert key value env.bindings }


{-| Recursively looks up a symbol in each of the scope, preferring inner scopes.
-}
lookupSymbol : SpSymbol -> Env -> Maybe SpExpression
lookupSymbol key (Env env) =
    case env.bindings |> Dict.get key of
        Just exp ->
            Just exp

        Nothing ->
            case env.parentScope of
                Nothing ->
                    Nothing

                Just parent ->
                    lookupSymbol key parent


{-| Pushes a scope onto the scope stack.
-}
pushScope : Env -> Env
pushScope env =
    Env { bindings = Dict.empty, parentScope = Just env }


{-| Pops a scope from the scope stack.
-}
popScope : Env -> Env
popScope (Env env) =
    case env.parentScope of
        Nothing ->
            Debug.todo "Cannot pop global scope"

        Just parent ->
            parent
