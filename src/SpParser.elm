module SpParser exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Set
import Types exposing (..)


{-| Parses an input string, potentially returning a parsed SpExpression.
-}
parseExpr : String -> Result String SpExpression
parseExpr input =
    Parser.run expressionParser input
        |> Result.mapError (always <| "Failed to parse input: " ++ input)


{-| Parses an input string, potentially returning a list of SpExpressions. All comments are stripped.
-}
parseFile : String -> Result String (List SpExpression)
parseFile input =
    Parser.run fileParser input
        |> Result.mapError (always <| "Failed to parse input: " ++ input)
        |> Result.map (List.filter ((/=) SpNothing))


whitespaceOrTabs =
    Parser.chompWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}')


{-| Parses an SpInt.
-}
intParser : Parser SpExpression
intParser =
    Parser.succeed SpInt
        |= Parser.oneOf
            [ Parser.succeed negate
                |. Parser.symbol "-"
                |= Parser.int
            , Parser.int
            ]
        |> Parser.backtrackable


{-| Parses an SpSymbol.
-}
symbolParser : Parser SpExpression
symbolParser =
    let
        allowedSymbolChars =
            "!@#$%^&*-+/~_?=<>" |> String.toList

        symbolStart c =
            Char.isAlpha c || List.member c allowedSymbolChars

        symbolBody c =
            Char.isAlphaNum c || List.member c allowedSymbolChars
    in
    Parser.succeed SpSymbol
        |= Parser.variable
            { start = symbolStart
            , inner = symbolBody
            , reserved = Set.empty
            }


{-| Parses an SpList, an S-Expression (a space-separated list surrounded by parens).
Note we don't consider anything for the separator as the expressionParser consumes whitespace for us.
-}
listParser : Parser SpExpression
listParser =
    Parser.succeed SpList
        |= Parser.sequence
            { start = "("
            , separator = ""
            , end = ")"
            , item = expressionParser
            , spaces = whitespaceOrTabs
            , trailing = Parser.Optional
            }


{-| Parser for shorthand quote expressions.
"'(1 2 3)" parses to the equivalent of "(quote (1 2 3))"
-}
quoteParser : Parser SpExpression
quoteParser =
    let
        wrapWith function body =
            SpList [ SpSymbol function, body ]

        makeParser symbol function =
            Parser.succeed (wrapWith function)
                |. Parser.symbol symbol
                |= expressionParser
    in
    Parser.oneOf
        [ makeParser "'" "quote"
        , makeParser "`" "quasiquote"
        , makeParser "," "unquote"
        ]


{-| Parses comments, yielding an SpNothing.
-}
commentParser : Parser SpExpression
commentParser =
    Parser.succeed SpNothing
        |. Parser.lineComment ";"


{-| Parses an arbitrary SpExpression including nested lists. Strips the whitespace surrounding the expression.
-}
expressionParser : Parser SpExpression
expressionParser =
    let
        lazy =
            Parser.lazy
                (\_ ->
                    Parser.oneOf [ intParser, symbolParser, listParser, quoteParser, commentParser ]
                )
    in
    Parser.succeed identity
        |. whitespaceOrTabs
        |= lazy
        |. whitespaceOrTabs


{-| Prints an expression out.
-}
print : SpExpression -> String
print expr =
    case expr of
        SpInt int ->
            String.fromInt int

        SpSymbol symbol ->
            symbol

        SpList list ->
            "(" ++ (list |> List.map print |> String.join " ") ++ ")"

        BuiltinFun _ ->
            "<builtin>"

        ClosureFun _ _ _ ->
            "<function>"

        SpNothing ->
            ""


{-| Parses a file which a sequence of expressions.
-}
fileParser : Parser (List SpExpression)
fileParser =
    Parser.sequence
        { start = ""
        , separator = ""
        , end = ""
        , item = expressionParser
        , spaces = whitespaceOrTabs
        , trailing = Parser.Optional
        }
