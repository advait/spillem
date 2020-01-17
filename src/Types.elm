module Types exposing (..)


type alias SpSymbol =
    String


type SpExpression
    = SpInt Int
    | SpSymbol SpSymbol
    | SpList (List SpExpression)
