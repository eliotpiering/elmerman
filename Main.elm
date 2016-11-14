module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Html.App as App
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Char
import Debug


-- Global Configs


velocity =
    15


screenHeight =
    550


screenWidth =
    650


numberOfColumns =
    13


numberOfRows =
    11


cellWidth =
    screenWidth / numberOfColumns


cellHeight =
    screenHeight / numberOfRows


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
    Svg.svg
        [ Svg.width <| toString screenWidth
        , Svg.height <| toString screenHeight
        ]
        (grid model
            ++ [ specialSquare model ]
            ++ [ player model ]
        )


player : Model -> Svg Msg
player model =
       Svg.image [Svg.xlinkHref forward, Svg.width "50", Svg.height "50", Svg.x (model.x - 25 |> toString), Svg.y (model.y - 25 |> toString)] []


grid : Model -> List (Svg Msg)
grid model =
    (List.map column [0..numberOfColumns]) |> List.concat


specialSquare : Model -> Svg Msg
specialSquare model =
    let
        ( colIndex, rowIndex ) =
            positionToCell model.x model.y
    in
        cell True colIndex rowIndex

positionToCell : Int -> Int -> (Int, Int)
positionToCell x y =
    (floor (toFloat x / cellWidth), floor (toFloat y / cellHeight))


-- Svg.rect
--     [ Svg.width <| toString screenWidth
--     , Svg.height <| toString screenHeight
--     , Svg.fill "yellow"
--     ]


column : Int -> List (Svg Msg)
column columnIndex =
    (List.map (cell False columnIndex) [0..numberOfRows])


cell : Bool -> Int -> Int -> Svg Msg
cell isOver columnIndex rowIndex =
    let
        xoffset =
            (toFloat columnIndex) * cellWidth

        yoffset =
            (toFloat rowIndex) * cellHeight
    in
        Svg.rect
            [ Svg.width (toString cellWidth)
            , Svg.height (toString cellHeight)
            , Svg.fill <| cellColor (columnIndex + rowIndex) isOver
            , Svg.y (toString yoffset)
            , Svg.x (toString xoffset)
            ]
            []


cellColor : Int -> Bool -> String
cellColor cellNum isOver =
    if isOver then
        "white"
    else if cellNum % 2 == 0 then
        "red"
    else
        "yellow"


----- Assets -----------

# TODO maybe a record type instead of functions for the different sprites
assetPath : String
assetPath  =
    "assets/"

forward : String
forward =
    assetPath ++ "forward.png"
