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
    { env : Env
    , lastResult : Result String SpExpression
    , history : List String
    , curInputValue : String
    }


init : Model
init =
    { env = Eval.defaultEnv
    , lastResult = Ok SpNothing
    , history = []
    , curInputValue = ""
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
                    { model
                        | history = model.history ++ [ s, "=>" ++ err ]
                        , curInputValue = ""
                    }

                Ok expr ->
                    let
                        evaluated =
                            Eval.eval model.env expr
                    in
                    { model
                        | env = evaluated.env
                        , lastResult = evaluated.result
                        , history = model.history ++ [ s, "=>" ++ evalResultToString evaluated.result ]
                        , curInputValue = ""
                    }


evalResultToString : Result String SpExpression -> String
evalResultToString evalResult =
    case evalResult of
        Err err ->
            err

        Ok expr ->
            SpParser.print expr



-- VIEW


view : Model -> Html Msg
view model =
    div bodyClass
        [ div [] (model.history |> List.map (\s -> div rowClass [ text s ]))
        , div rowClass [ input (inputClass ++ [ sendStringOnEnter, value model.curInputValue ]) [] ]
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
