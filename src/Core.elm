module Core exposing (..)

import SpParser


{-| Spillem's core library that consists of Spillem-defined functions that are composed of themselves and Builtins.
-}
spillem =
    """
;; Returns whether the given list is empty.
(def! empty?
  (fn* (l)
    (= l ())
  )
)
    """


core =
    case spillem |> String.trim |> SpParser.parseFile of
        Ok ok ->
            ok

        Err err ->
            Debug.todo err
