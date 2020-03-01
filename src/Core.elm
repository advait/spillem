module Core exposing (..)

import SpParser


{-| Spillem's core library that consists of Spillem-defined functions that are composed of themselves and Builtins.
-}
spillem =
    """
;;; Boolean Operations

;; Boolean not
(def! not
  (fn* (b) (if b false true))
)

;; Boolean or
(def! or
  (fn* (a b)
    (if a b false)
  )
)

;; Boolean and
(def! and
  (fn* (a b)
    (if (not a) false b)
  )
)


;;; List operations

;; Returns whether the given list is empty.
(def! empty?
  (fn* (l)
    (= l ())
  )
)

;; Returns whether the argument is a list.
(def! list?
  (fn* (l)
    (= (quote List) (type l))
  )
)

;; The famous Y Combinator! Used to enable recurision in non-recursive languages.
(def! Y
   (fn* (X)
     ((fn* (procedure)
        (X (fn* (arg) ((procedure procedure) arg))))
      (fn* (procedure)
        (X (fn* (arg) ((procedure procedure) arg)))))
   )
)
 
; Fib
(def! Fib*
  (fn* (Fib*)
    (fn* (n)
      (if (< n 2)
          n
          (+ (Fib* (- n 1)) (Fib* (- n 2)))
      )
    )
  )
)

(def! fib (Y Fib*))
    """


core =
    case spillem |> String.trim |> SpParser.parseFile of
        Ok ok ->
            ok

        Err err ->
            Debug.todo err
