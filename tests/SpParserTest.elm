module SpParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import SpParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Parser"
        [ test "parses digit 0" <| \_ -> Expect.equal (Just <| SpInt 0) (doParse "0")
        , test "parses digits 120" <| \_ -> Expect.equal (Just <| SpInt 120) (doParse "120")
        , test "parses digits -20" <| \_ -> Expect.equal (Just <| SpInt -20) (doParse "-20")
        ]


doParse : String -> Maybe SpExpression
doParse input =
    parse input |> Result.toMaybe
