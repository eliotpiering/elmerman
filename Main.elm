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
import List.Extra as ListEx


-- Global Configs


type alias Config =
    { velocity : Float
    , spriteChangeRate : Int
    , screenHeight : Int
    , screenWidth : Int
    , numberOfColumns : Int
    , numberOfRows : Int
    }


config =
    { velocity = 0.22
    , spriteChangeRate = 8
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
    { x : Int
    , y : Int
    , direction : Direction
    , currentSprite : Int
    , bombs : List Bomb
    , bricks : List Brick
    }


type alias Bomb =
    { x : Int
    , y : Int
    , currentSprite : Int
    , timer : Int
    }


type alias Brick =
    { x : Int
    , y : Int
    }


type alias Obstacle =
    -- A generic model used in the move function
    -- This could be a Brick or a Bomb
    { x : Int
    , y : Int
    }


toObstactle item =
    { x = item.x, y = item.y }


type Direction
    = Left
    | Right
    | Up
    | Down
    | NoDirection


initialModel : Model
initialModel =
    { x = halfSpriteWidth
    , y = halfSpriteHeight
    , direction = NoDirection
    , currentSprite = 0
    , bombs = []
    , bricks = [ { x = 2, y = 0 }, { x = 2, y = 1 }, { x = 2, y = 2 }, { x = 0, y = 2 }, { x = 1, y = 2 } ]
    }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timeDiff ->
            noCmds
                (model
                    |> move timeDiff
                    |> updateBombs timeDiff
                )

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
                32 ->
                    -- Space Bar --
                    dropBomb model

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


updateSpriteNumber : Int -> Int
updateSpriteNumber number =
    -- have to use spriteChangeRate parameter to slow down the rate that the sprite changes
    let
        numberOfSprites =
            3
    in
        if number >= (numberOfSprites * config.spriteChangeRate - 1) then
            -- reset back to 0
            0
        else
            -- increate the sprite number
            number + 1


updateBombs : Time -> Model -> Model
updateBombs timeDiff model =
    { model
        | bombs =
            model.bombs
                |> List.map
                    (\bomb ->
                        { bomb
                            | timer = bomb.timer - (round timeDiff)
                            , currentSprite = updateSpriteNumber bomb.currentSprite
                        }
                    )
                |> List.filter
                    (\bomb ->
                        bomb.timer > 0
                    )
    }


move : Time -> Model -> Model
move timeDiff model =
    let
        changeInPosition =
            timeDiff * config.velocity |> round

        ( currentCellX, currentCellY ) =
            positionToCell ( model.x, model.y )

        obstaclesInColumn =
            List.append (List.map toObstactle model.bombs) (List.map toObstactle model.bricks) |> List.filter (\obs -> obs.x == currentCellX)

        obstaclesInRow =
            List.append (List.map toObstactle model.bombs) (List.map toObstactle model.bricks) |> List.filter (\obs -> obs.y == currentCellY)
    in
        case model.direction of
            Left ->
                let
                    maybeReleventObsBorder =
                        -- first filter any obs to the right of currentCellX
                        -- then get obs with the higest X (or Nothing)
                        -- finally get the position of the right edge of that obs
                        obstaclesInRow
                            |> List.filter (\obs -> obs.x < currentCellX)
                            |> ListEx.maximumBy (\obs -> obs.x)
                            |> Maybe.andThen (\obs -> Just (((obs.x + 1) * cellWidth) + 1))

                    leftBorder =
                        Maybe.withDefault halfSpriteWidth maybeReleventObsBorder

                    newX =
                        max leftBorder (model.x - changeInPosition)
                in
                    { model
                        | x = newX
                        , currentSprite = updateSpriteNumber model.currentSprite
                    }

            Right ->
                let
                    maybeReleventObsBorder =
                        -- first filter any obs to the left of currentCellX
                        -- then get obs with the lowest X (or Nothing)
                        -- finally get the position of the right edge of that obs
                        obstaclesInRow
                            |> List.filter (\obs -> obs.x > currentCellX)
                            |> ListEx.minimumBy (\obs -> obs.x)
                            |> Maybe.andThen (\obs -> Just ((obs.x * cellWidth) - 1))

                    rightBorder =
                        Maybe.withDefault (config.screenWidth - halfSpriteWidth) maybeReleventObsBorder

                    newX =
                        min rightBorder (model.x + changeInPosition)
                in
                    { model
                        | x = newX
                        , currentSprite = updateSpriteNumber model.currentSprite
                    }

            Up ->
                let
                    maybeReleventObsBorder =
                        -- first filter any obs below currentCellY
                        -- then get obs with the lowest Y (or Nothing)
                        -- finally get the position of the bottom edge of that obs
                        obstaclesInColumn
                            |> List.filter (\obs -> obs.y < currentCellY)
                            |> ListEx.maximumBy (\obs -> obs.y)
                            |> Maybe.andThen (\obs -> Just (((obs.y + 1) * cellHeight) + 1))

                    topBorder =
                        Maybe.withDefault halfSpriteHeight maybeReleventObsBorder

                    newY =
                        max topBorder (model.y - changeInPosition)
                in
                    { model
                        | y = newY
                        , currentSprite = updateSpriteNumber model.currentSprite
                    }

            Down ->
                let
                    maybeReleventObsBorder =
                        -- first filter any obs above currentCellY
                        -- then get obs with the highest Y (or Nothing)
                        -- finally get the position of the top edge of that obs
                        obstaclesInColumn
                            |> List.filter (\obs -> obs.y > currentCellY)
                            |> ListEx.minimumBy (\obs -> obs.y)
                            |> Maybe.andThen (\obs -> Just ((obs.y * cellHeight) - 1))

                    botomBorder =
                        Maybe.withDefault (config.screenHeight - halfSpriteHeight) maybeReleventObsBorder

                    newY =
                        min botomBorder (model.y + changeInPosition)
                in
                    { model
                        | y = newY
                        , currentSprite = updateSpriteNumber model.currentSprite
                    }

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


dropBomb : Model -> ( Model, Cmd Msg )
dropBomb model =
    let
        ( cellX, cellY ) =
            positionToCell ( model.x, model.y )

        newBomb =
            { x = cellX, y = cellY, currentSprite = 0, timer = 5000 }
    in
        noCmds { model | bombs = newBomb :: model.bombs }


subscriptions : Model -> Sub Msg
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
            ++ (renderBombs model.bombs)
            ++ (renderBricks model.bricks)
        )


renderBricks : List Brick -> List (Svg Msg)
renderBricks =
    let
        brickWidth =
            cellWidth |> toString

        brickHeight =
            cellHeight |> toString
    in
        List.map
            (\brick -> renderBrick "brown" brick.x brick.y)


renderBombs : List Bomb -> List (Svg Msg)
renderBombs =
    let
        bombWidth =
            cellWidth |> toString

        bombHeight =
            cellHeight |> toString
    in
        List.map
            (\bomb ->
                let
                    ( bombX, bombY ) =
                        cellToPosition ( bomb.x, bomb.y )

                    spriteImage =
                        getBombSprite bomb
                in
                    Svg.image [ Svg.xlinkHref spriteImage, Svg.width bombWidth, Svg.height bombHeight, Svg.x (toString bombX), Svg.y (toString bombY) ] []
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

        spriteImage =
            getPlayerSprite model
    in
        Svg.image [ Svg.xlinkHref spriteImage, Svg.width playerWidth, Svg.height playerHeight, Svg.x centerX, Svg.y centerY ] []


grid : Model -> List (Svg Msg)
grid model =
    (List.map column <| List.range 0 config.numberOfColumns) |> List.concat


specialSquare : Model -> Svg Msg
specialSquare model =
    let
        ( colIndex, rowIndex ) =
            positionToCell ( model.x, model.y )
    in
        cell True colIndex rowIndex


positionToCell : ( Int, Int ) -> ( Int, Int )
positionToCell ( x, y ) =
    ( x // cellWidth, y // cellHeight )


cellToPosition : ( Int, Int ) -> ( Int, Int )
cellToPosition ( x, y ) =
    ( cellWidth * x, cellHeight * y )


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


renderBrick : String -> Int -> Int -> Svg Msg
renderBrick color columnIndex rowIndex =
    -- TODO this is the same as the cell function except takes a color instead of a bool
    -- there is probably a way to combine these two functions
    let
        xoffset =
            columnIndex * cellWidth

        yoffset =
            rowIndex * cellHeight
    in
        Svg.rect
            [ Svg.width (toString cellWidth)
            , Svg.height (toString cellHeight)
            , Svg.fill color
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


getPlayerSprite : Model -> String
getPlayerSprite model =
    case model.direction of
        Left ->
            assetPath ++ "left" ++ (toString <| model.currentSprite // config.spriteChangeRate) ++ ".png"

        Right ->
            assetPath ++ "right" ++ (toString <| model.currentSprite // config.spriteChangeRate) ++ ".png"

        Up ->
            assetPath ++ "back" ++ (toString <| model.currentSprite // config.spriteChangeRate) ++ ".png"

        Down ->
            assetPath ++ "forward" ++ (toString <| model.currentSprite // config.spriteChangeRate) ++ ".png"

        NoDirection ->
            assetPath ++ "forward0.png"


getBombSprite : Bomb -> String
getBombSprite bomb =
    let
        bombNumber =
            bomb.currentSprite // config.spriteChangeRate |> toString
    in
        assetPath ++ "bomb" ++ bombNumber ++ ".png"



-- "assets/bomb0.png"


assetPath : String
assetPath =
    "assets/"
