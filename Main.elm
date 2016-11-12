module Main exposing (..)

import Html exposing (Html)
import Html.App as App
import Keyboard exposing (KeyCode)
import Svg
import Svg.Attributes as Svg
import Char
import Debug


main =
    App.program
        { init = noCmds initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


noCmds model =
    ( model, Cmd.none )


type alias Model =
    { x : Int, y : Int }


initialModel : Model
initialModel =
    { x = 0, y = 0 }


type Msg
    = KeyPress KeyCode

velocity = 2

update msg model =
    case msg of
        KeyPress 37 ->
            -- Left
            noCmds { model | x = model.x - velocity }

        KeyPress 39 ->
            -- Right
            noCmds { model | x = model.x + velocity }

        KeyPress 40 ->
            -- Down
            noCmds { model | y = model.y + velocity }

        KeyPress 38 ->
            -- Up
            noCmds { model | y = model.y - velocity }

        _ ->
            noCmds model


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyPress ]


view model =
    Svg.svg []
    [Svg.circle [ Svg.cx (toString model.x), Svg.cy (toString model.y), Svg.r "50" ] []]
