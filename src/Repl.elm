module Repl exposing (..)

import Browser
import Eval
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (autofocus, style, value)
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import SpParser
import Types exposing (..)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { lastResult : EvalResult
    , history : List String
    , curValue : String
    }


init : Model
init =
    { lastResult = { env = Eval.defaultEnv, result = Ok <| SpInt 99 }
    , history = []
    , curValue = ""
    }



-- UPDATE


type Msg
    = SendValue String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SendValue s ->
            case SpParser.parseExpr s of
                Err err ->
                    { model | history = model.history ++ [ s, "=>" ++ err ], curValue = "" }

                Ok expr ->
                    let
                        evaluated =
                            Eval.eval model.lastResult.env expr
                    in
                    { model
                        | history = model.history ++ [ s, "=>" ++ Debug.toString evaluated.result ]
                        , lastResult = evaluated
                        , curValue = ""
                    }



-- VIEW


view : Model -> Html Msg
view model =
    div bodyClass
        [ div [] (model.history |> List.map (\s -> div rowClass [ text s ]))
        , div rowClass [ input (inputClass ++ [ sendStringOnEnter, value model.curValue ]) [] ]
        ]


sendStringOnEnter : Attribute Msg
sendStringOnEnter =
    let
        isEnter code =
            if code == 13 then
                Debug.log "keys" <| Json.succeed ()

            else
                Json.fail "not ENTER"

        getText =
            Json.at [ "target", "value" ] Json.string
                |> Json.map SendValue

        both =
            Json.map2 (\_ save -> save) (keyCode |> Json.andThen isEnter) getText
    in
    on "keydown" both


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
    [ style "padding-top" "2px" ]


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
