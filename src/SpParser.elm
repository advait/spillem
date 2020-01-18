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
    Parser.succeed SpSymbol
        |= Parser.variable
            { start = \c -> Char.isAlpha c
            , inner = \c -> Char.isAlphaNum c || c == '_'
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
