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
    }


config =
    { velocity = 15
    , screenHeight = 550
    , screenWidth = 650
    , numberOfColumns = 13
    , numberOfRows = 11
    }


cellWidth : Int
cellWidth =
    config.screenWidth // config.numberOfRows

cellHeight : Int
cellHeight =
    config.screenHeight // config.numberOfColumns

halfSpriteWidth : Int
halfSpriteWidth =
    cellWidth // 2

halfSpriteHeight : Int
halfSpriteHeight =
    cellHeight // 2


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
    { x = halfSpriteWidth, y = halfSpriteHeight }


type Msg
    = KeyPress KeyCode


update msg model =
    case msg of
        KeyPress 37 ->
            -- Left
            noCmds { model | x = max halfSpriteWidth (model.x - config.velocity) }

        KeyPress 39 ->
            -- Right
            -- noCmds { model | x = min (model.x + config.velocity) (850 - halfSpriteWidth) }
            noCmds { model | x = min (model.x + config.velocity) (config.screenWidth - halfSpriteWidth) }

        KeyPress 40 ->
            -- Down
            noCmds { model | y = min (model.y + config.velocity) (config.screenHeight - halfSpriteHeight) }

        KeyPress 38 ->
            -- Up
            noCmds { model | y = max (model.y - config.velocity) (0 + halfSpriteHeight) }

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
            cellWidth |> toString

        playerHeight =
            cellHeight |> toString

        centerX =
            (model.x - (cellWidth // 2)) |> toString

        centerY =
            (model.y - (cellHeight // 2)) |> toString
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
    (x // cellWidth, y // cellHeight)



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
            columnIndex * cellWidth

        yoffset =
            rowIndex * cellHeight
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
-- TODO maybe a record type instead of functions for the different sprites


assetPath : String
assetPath =
    "assets/"


forward : String
forward =
    assetPath ++ "forward.png"
