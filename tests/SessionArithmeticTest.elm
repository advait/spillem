module SessionArithmeticTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing REPL_ENV
(+ 3 2)
;=>5
(/ (- (+ 5 (* 2 3)) 3) 4)
;=>2
(/ (- (+  5 (  * 2  3 ) )  3 ) 4 )
;=>2

(fib 6)
;=>8
""" |> String.trim


suite : Test
suite =
    describe "Basic Arithmetic" <| SessionTestHelper.parseSession spillem
