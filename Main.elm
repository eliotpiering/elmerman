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


type alias Config =
    { velocity : Int
    , screenHeight : Int
    , screenWidth : Int
    , numberOfColumns : Int
    , numberOfRows : Int
    , cellWidth : Float
    , cellHeight : Float
    }


config =
    { velocity = 15
    , screenHeight = 750
    , screenWidth = 850
    , numberOfColumns = 13
    , numberOfRows = 11
    , cellWidth = 850 / 13
    , cellHeight = 750 / 11
    }


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
            noCmds { model | x = model.x - config.velocity }

        KeyPress 39 ->
            -- Right
            noCmds { model | x = model.x + config.velocity }

        KeyPress 40 ->
            -- Down
            noCmds { model | y = model.y + config.velocity }

        KeyPress 38 ->
            -- Up
            noCmds { model | y = model.y - config.velocity }

        _ ->
            noCmds model


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyPress ]


view model =
    Svg.svg
        [ Svg.width <| toString config.screenWidth
        , Svg.height <| toString config.screenHeight
        ]
        (grid model
            ++ [ specialSquare model ]
            ++ [ player model ]
        )


player : Model -> Svg Msg
player model =
    let
        playerWidth =
            config.cellWidth |> toString

        playerHeight =
            config.cellHeight |> toString

        centerX =
            ((toFloat model.x) - (config.cellWidth / 2)) |> toString

        centerY =
            ((toFloat model.y) - (config.cellHeight / 2)) |> toString
    in
        Svg.image [ Svg.xlinkHref forward, Svg.width playerWidth, Svg.height playerHeight, Svg.x centerX, Svg.y centerY ] []


grid : Model -> List (Svg Msg)
grid model =
    (List.map column [0..config.numberOfColumns]) |> List.concat


specialSquare : Model -> Svg Msg
specialSquare model =
    let
        ( colIndex, rowIndex ) =
            positionToCell model.x model.y
    in
        cell True colIndex rowIndex


positionToCell : Int -> Int -> ( Int, Int )
positionToCell x y =
    ( floor (toFloat x / config.cellWidth), floor (toFloat y / config.cellHeight) )



-- Svg.rect
--     [ Svg.width <| toString config.screenWidth
--     , Svg.height <| toString config.screenHeight
--     , Svg.fill "yellow"
--     ]


column : Int -> List (Svg Msg)
column columnIndex =
    (List.map (cell False columnIndex) [0..config.numberOfRows])


cell : Bool -> Int -> Int -> Svg Msg
cell isOver columnIndex rowIndex =
    let
        xoffset =
            (toFloat columnIndex) * config.cellWidth

        yoffset =
            (toFloat rowIndex) * config.cellHeight
    in
        Svg.rect
            [ Svg.width (toString config.cellWidth)
            , Svg.height (toString config.cellHeight)
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
-- TODO maybe a record type instead of functions for the different sprites


assetPath : String
assetPath =
    "assets/"


forward : String
forward =
    assetPath ++ "forward.png"
