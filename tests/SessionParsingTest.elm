module SessionParsingTest exposing (..)

import SessionTestHelper
import Test exposing (Test, describe)


spillem =
    """
;; Testing Parsing
1
;=>1
()
;=>()
(  )
;=>()
 2
;=>2
( + 1  2 )
;=>3
""" |> String.trim


suite : Test
suite =
    describe "Basic Expression Parsing" <| SessionTestHelper.parseSession spillem
