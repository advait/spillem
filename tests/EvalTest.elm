module EvalTest exposing (..)

import Eval exposing (eval)
import Expect
import SpParser exposing (parse, print)
import Stdlib
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "eval"
        [ testBasic "5" (SpInt 5)
        , testBasic "()" (SpList [])
        , testBasic "+" (BuiltinFun Stdlib.plus)
        , testBasic "(+ 1 2)" (SpInt 3)
        , testBasic "(+ 1 (+ 2 3))" (SpInt 6)
        , testBasic "(+ (+ 4 5) (+ 2 3))" (SpInt 14)
        ]


defaultEnv =
    Stdlib.lib


testBasic : String -> SpExpression -> Test
testBasic input expected =
    let
        desc =
            "'" ++ input ++ "' evaluates to '" ++ print expected ++ "'"
    in
    test desc <|
        \_ ->
            case parse input of
                Err s ->
                    Expect.fail s

                Ok parsedInput ->
                    Expect.equal (eval defaultEnv parsedInput) (Ok expected)
