module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Html
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Char
import Debug
import Time exposing (Time)
import AnimationFrame


-- Global Configs


type alias Config =
    { velocity : Float
    , screenHeight : Int
    , screenWidth : Int
    , numberOfColumns : Int
    , numberOfRows : Int
    }


config =
    { velocity = 0.3
    , screenHeight = 550
    , screenWidth = 650
    , numberOfColumns = 13
    , numberOfRows = 11
    }


cellWidth : Int
cellWidth =
    config.screenWidth // config.numberOfColumns


cellHeight : Int
cellHeight =
    config.screenHeight // config.numberOfRows


halfSpriteWidth : Int
halfSpriteWidth =
    cellWidth // 2


halfSpriteHeight : Int
halfSpriteHeight =
    cellHeight // 2


main =
    Html.program
        { init = noCmds initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


noCmds model =
    ( model, Cmd.none )


type alias Model =
    { x : Int, y : Int, direction : Direction }


type Direction
    = Left
    | Right
    | Up
    | Down
    | NoDirection


initialModel : Model
initialModel =
    { x = halfSpriteWidth, y = halfSpriteHeight, direction = NoDirection }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick timeDiff ->
            noCmds (move model timeDiff)

        KeyUp code ->
            case code of
                37 ->
                    turnOff model Left

                39 ->
                    turnOff model Right

                40 ->
                    turnOff model Down

                38 ->
                    turnOff model Up

                _ ->
                    noCmds model

        KeyDown code ->
            case code of
                37 ->
                    turnOn model Left

                39 ->
                    turnOn model Right

                40 ->
                    turnOn model Down

                38 ->
                    turnOn model Up

                _ ->
                    noCmds model


move : Model -> Time -> Model
move model timeDiff =
    let
        changeInPosition =
            timeDiff * config.velocity |> round
    in
        case model.direction of
            Left ->
                { model | x = max halfSpriteWidth (model.x - changeInPosition) }

            Right ->
                { model | x = min (config.screenWidth - halfSpriteWidth) (model.x + changeInPosition) }

            Up ->
                { model | y = max halfSpriteHeight (model.y - changeInPosition) }

            Down ->
                { model | y = min (config.screenHeight - halfSpriteHeight) (model.y + changeInPosition) }

            NoDirection ->
                model


turnOn : Model -> Direction -> ( Model, Cmd Msg )
turnOn model direction =
    noCmds { model | direction = direction }

turnOff : Model -> Direction -> ( Model, Cmd Msg )
turnOff model direction =
    if model.direction == direction then
        noCmds { model | direction = NoDirection }
    else
        noCmds model


subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.diffs Tick
        ]


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
    (List.map column <| List.range 0 config.numberOfColumns) |> List.concat


specialSquare : Model -> Svg Msg
specialSquare model =
    let
        ( colIndex, rowIndex ) =
            positionToCell model.x model.y
    in
        cell True colIndex rowIndex


positionToCell : Int -> Int -> ( Int, Int )
positionToCell x y =
    ( x // cellWidth, y // cellHeight )



-- Svg.rect
--     [ Svg.width <| toString config.screenWidth
--     , Svg.height <| toString config.screenHeight
--     , Svg.fill "yellow"
--     ]


column : Int -> List (Svg Msg)
column columnIndex =
    (List.map (cell False columnIndex) <| List.range 0 config.numberOfRows)


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
