module SpParserTest exposing (..)

import Expect exposing (Expectation)
import SpParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Parser"
        [ describe "int"
            [ testParse (SpInt 0) "0"
            , testParse (SpInt 120) "120"
            , testParse (SpInt -2) "-2"
            , testParse (SpInt 0) "-0"
            , testParseFail "0x1a" -- Hex not supported
            ]
        , describe "symbol"
            [ testParse (SpSymbol "a") "a"
            , testParse (SpSymbol "a_a") "a_a"
            , testParse (SpSymbol "Nil") "Nil"
            , testParseFail "#t" -- # symbols not supported yet
            ]
        , describe "list"
            [ testParse (SpList []) "()"
            , testParse (SpList [ SpInt 1 ]) "(1)"
            , testParse (SpList [ SpList [] ]) "(())"
            , testParse (SpList [ SpInt 2, SpList [] ]) "(2 ())"
            ]
        ]


testParse : SpExpression -> String -> Test
testParse out input =
    test ("Parses " ++ input) <| \_ -> Expect.equal (Just out) (doParse input)


testParseFail input =
    test ("Doesn't Parse " ++ input) <| \_ -> Expect.equal Nothing (doParse input)


doParse : String -> Maybe SpExpression
doParse input =
    parse input |> Result.toMaybe
