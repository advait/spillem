module EvalTest exposing (..)

import Eval exposing (eval, evalAll)
import Expect
import SpParser exposing (parseExpr, parseFile, print)
import Stdlib
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "eval"
        [ describe "evalExpr"
            [ testSingle "5" (SpInt 5)
            , testSingle "()" (SpList [])
            , testSingle "(+ 1 2)" (SpInt 3)
            , testSingle "(+ 1 (+ 2 3))" (SpInt 6)
            , testSingle "(+ (+ 4 5) (+ 2 3))" (SpInt 14)
            , testSingle "(- 0 2)" (SpInt -2)
            , testSingle "(- (* 3 4) (/ 9 3))" (SpInt 9)
            , testSingle "(/ (- (+ 5 (* 2 3)) 3) 4)" (SpInt 2)
            ]
        , describe "evalAll"
            [ testAll "()()" (SpList [])
            , testAll "()5" (SpInt 5)
            , testAll "(def! x 3)" (SpInt 3)
            , testAll "(def! y (+ 1 7)) y" (SpInt 8)
            , testAll "(def! mynum 111) (def! MYNUM 222) mynum" (SpInt 111)
            , testAll "(def! mynum 111) (def! MYNUM 222) MYNUM" (SpInt 222)
            ]
        ]


defaultEnv =
    Stdlib.lib


{-| Test the evaluation of a single expression, ignoring the environment.
-}
testSingle : String -> SpExpression -> Test
testSingle input expected =
    let
        desc =
            "'" ++ input ++ "' evaluates to '" ++ print expected ++ "'"
    in
    test desc <|
        \_ ->
            case parseExpr input of
                Err s ->
                    Expect.fail s

                Ok parsedInput ->
                    Expect.equal (eval defaultEnv parsedInput |> .result) (Ok expected)


{-| Test the evaluation of a sequence of expressions, asserting the final value matches the provided SpExpression.
-}
testAll : String -> SpExpression -> Test
testAll input expected =
    let
        desc =
            "'" ++ input ++ "' evaluates to '" ++ print expected ++ "'"
    in
    test desc <|
        \_ ->
            case parseFile input of
                Err s ->
                    Expect.fail s

                Ok parsedInput ->
                    Expect.equal (evalAll defaultEnv parsedInput |> .result) (Ok expected)
