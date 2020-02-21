module SessionIfTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing if statements
(if 1 2 3)
;=>2

(if true 2 3)
;=>2

(if false 2 3)
;=>3

(if nil 2 3)
;=>3

(if true 2 should-explode)
;=>2

(if false should-explode 3)
;=>3
""" |> String.trim


suite : Test
suite =
    describe "If Statements" <| SessionTestHelper.parseSession spillem
