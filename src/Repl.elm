module Repl exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (autofocus, style)
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode as Json



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : Model -> Html Msg
view model =
    div bodyClass
        [ div [] [ text (String.fromInt model) ]
        , div rowClass [ text "Hello World" ]
        , div rowClass [ input (inputClass ++ [ onEnter Increment ]) [] ]
        ]


colorClass =
    [ style "background-color" "#26292C"
    , style "color" "#cfd2d0"
    ]


fontClass =
    [ style "font-family" "\"Droid Sans Mono\""
    , style "font-size" "14px"
    ]


bodyClass : List (Attribute msg)
bodyClass =
    colorClass
        ++ [ style "font-family" "\"Droid Sans Mono\""
           , style "font-size" "14px"
           , style "background-color" "#26292C"
           , style "color" "#cfd2d0"
           , style "position" "absolute"
           , style "top" "0"
           , style "bottom" "0"
           , style "left" "0"
           , style "right" "0"
           , style "padding-top" "5px"
           , style "padding-left" "5px"
           ]


rowClass : List (Attribute msg)
rowClass =
    []


inputClass =
    colorClass
        ++ fontClass
        ++ [ style "outline" "none"
           , style "border" "none"
           , style "border" "none"
           , style "padding" "0"
           , style "margin" "0"
           , autofocus True
           ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
