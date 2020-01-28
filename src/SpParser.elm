module SpParser exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Set
import Types exposing (..)


{-| Parses an input string, potentially returning a parsed SpExpression.
-}
parse : String -> Result ParseError SpExpression
parse input =
    Parser.run expressionParser input
        |> Result.mapError (always ParseError)


type ParseError
    = ParseError


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


{-| Parses an SpSymbol.
-}
symbolParser : Parser SpExpression
symbolParser =
    let
        allowedSymbolChars =
            "!@#$%^&*-+/,`~_" |> String.toList

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
-}
listParser : Parser SpExpression
listParser =
    Parser.succeed SpList
        |= Parser.sequence
            { start = "("
            , separator = " "
            , end = ")"
            , item = expressionParser
            , spaces = Parser.succeed ()
            , trailing = Parser.Optional
            }


{-| Parses an arbitrary SpExpression including nested lists.
-}
expressionParser : Parser SpExpression
expressionParser =
    Parser.lazy
        (\_ ->
            Parser.oneOf [ intParser, symbolParser, listParser ]
        )


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
