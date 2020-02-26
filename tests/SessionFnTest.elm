module SessionFnTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
((fn* (a) a) 1)
;=>1

( (fn* (a) (+ a 1)) 10)
;=>11

( (fn* (a b) (+ a b)) 2 3)
;=>5

( (fn* () 4) )
;=>4

( ( (fn* () (fn* () 1) ) ) )
;=>1

( (fn* (f x) (f x)) (fn* (a) (+ 1 a)) 9)
;=>10

;; Testing closures
( ( (fn* (a) (fn* (b) (+ a b))) 5) 7)
;=>12

(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))
(def! plus5 (gen-plus5))
(plus5 7)
;=>12
    """ |> String.trim


suite : Test
suite =
    describe "fn* definitions" <| SessionTestHelper.parseSession spillem
