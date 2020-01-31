module SpParserTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import SpParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "parseExpr"
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
            , testParse (SpSymbol "#t") "#t"
            , testParse (SpSymbol "+") "+"
            , testParse (SpSymbol "-") "-"
            ]
        , describe "list"
            [ testParse (SpList []) "()"
            , testParse (SpList [ SpInt 1 ]) "(1)"
            , testParse (SpList [ SpInt 1 ]) "(1 )"
            , testParse (SpList [ SpInt 1 ]) "( 1 )"
            , testParse (SpList [ SpList [] ]) "(())"
            , testParse (SpList [ SpList [] ]) " ( ( ) ) "
            , testParse (SpList [ SpInt 2, SpList [] ]) "(2 ())"
            , testParse (SpList [ SpSymbol "-", SpInt 0, SpInt 2 ]) "(- 0 2)"
            ]
        , fuzz expFuzzer "parse and print are inverses" (\exp -> Expect.equal (Just exp) (doParse <| print exp))
        ]


testParse : SpExpression -> String -> Test
testParse out input =
    let
        actualTest input_ =
            test ("Parses '" ++ input_ ++ "'") <| \_ -> Expect.equal (Just out) (doParse input_)
    in
    [ input, " " ++ input, input ++ " ", "  " ++ input, input ++ "  ", "\t" ++ input, input ++ "\t" ]
        |> List.map actualTest
        |> Test.concat


testParseFail input =
    test ("Doesn't Parse " ++ input) <| \_ -> Expect.equal Nothing (doParse input)


doParse : String -> Maybe SpExpression
doParse input =
    parseExpr input |> Result.toMaybe


{-| Randomly generates expressions for fuzz testing.
-}
expFuzzer : Fuzzer SpExpression
expFuzzer =
    let
        intFuzzer =
            Fuzz.int |> Fuzz.map SpInt

        symbolFuzzer =
            Fuzz.string |> Fuzz.map (\s -> SpSymbol ("a" ++ s |> String.filter Char.isAlpha))

        -- Note that we must artificially constrain the depth of the list to avoid infinitely deep exprs
        listFuzzer maxDepth =
            if maxDepth <= 0 then
                Fuzz.oneOf [ intFuzzer, symbolFuzzer ]

            else
                Fuzz.oneOf [ intFuzzer, symbolFuzzer, listFuzzer (maxDepth - 1) ]
    in
    Fuzz.oneOf [ intFuzzer, symbolFuzzer, listFuzzer 3 ]
