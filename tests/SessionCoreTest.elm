module SessionCoreTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing Core
(empty? ())
;=>true
""" |> String.trim


suite : Test
suite =
    describe "Core Library" <| SessionTestHelper.parseSession spillem
