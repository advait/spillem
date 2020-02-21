module SessionDoTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing do statements
(do 1 2 3)
;=>3

(do (+ 1 1) (+ 2 2))
;=>4
""" |> String.trim


suite : Test
suite =
    describe "Do Statements" <| SessionTestHelper.parseSession spillem
