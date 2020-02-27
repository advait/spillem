module SessionCoreTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing Core
(empty? ())
;=>true

(empty? 1)
;=>false

(quote (1 2 3))
;=>(1 2 3)

(empty? (quote (1 2 3)))
;=>false
""" |> String.trim


suite : Test
suite =
    describe "Core Library" <| SessionTestHelper.parseSession spillem
