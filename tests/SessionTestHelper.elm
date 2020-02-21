module SessionTestHelper exposing (parseSession)

import Eval
import Expect exposing (Expectation)
import Parser exposing ((|.), (|=), Parser)
import SpParser
import Test exposing (Test, test)
import Types exposing (..)


{-| Helper function that parses a Spillem program with assertion comments and
returns a list of Tests corresponding to each assertion.
-}
parseSession : String -> List Test
parseSession input =
    let
        reduceSession : List Expectation -> SpState -> List SessionItem -> List Expectation
        reduceSession acc state sess =
            case sess of
                [] ->
                    acc |> List.reverse

                (Assertion ass) :: tail ->
                    let
                        expectation =
                            Expect.equal state.result (Ok ass)
                    in
                    reduceSession (expectation :: acc) state tail

                (Expr expr) :: tail ->
                    let
                        nextState =
                            Eval.eval expr state
                    in
                    case nextState.result of
                        Err err ->
                            let
                                newTest =
                                    Expect.fail err
                            in
                            reduceSession (newTest :: acc) nextState tail

                        Ok _ ->
                            reduceSession acc nextState tail

                (Description _) :: tail ->
                    -- TODO(advait): Support parsing descriptions
                    reduceSession acc state tail
    in
    case Parser.run sessionParser input of
        Err _ ->
            [ Test.test "Failed to parse" (\_ -> Expect.fail "Failed to parse") ]

        Ok session ->
            let
                expectations =
                    reduceSession [] Eval.initState session
            in
            expectations
                |> List.indexedMap
                    (\i expectation ->
                        test (String.fromInt i) (\_ -> expectation)
                    )


type SessionItem
    = Description String
    | Assertion SpExpression
    | Expr SpExpression


sessionParser : Parser (List SessionItem)
sessionParser =
    Parser.succeed identity
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , item = itemParser
            , spaces = SpParser.whitespaceOrTabs
            , trailing = Parser.Optional
            }
        |. Parser.end


itemParser : Parser SessionItem
itemParser =
    Parser.oneOf [ assertParser, descParser, exprParser ]


assertParser =
    Parser.succeed Assertion
        |. Parser.symbol ";=>"
        |= SpParser.expressionParser


descParser =
    Parser.succeed Description
        |. Parser.symbol ";; "
        |= Parser.getChompedString (Parser.chompUntilEndOr "\n")


exprParser : Parser SessionItem
exprParser =
    Parser.succeed Expr
        |= SpParser.expressionParser
