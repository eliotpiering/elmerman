module Main exposing (..)

import Html exposing (Html)
import Keyboard exposing (KeyCode)
import Svg exposing (Svg)
import Svg.Attributes as Svg
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


config : Config
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
    -- TODO make player its own model
    { player : Player
    , currentSprite : Int
    , bombs : List Bomb
    , explodingBombs : List Bomb
    , bricks : List Point
    , fires : List Point
    }


type alias Player =
    { x : Int
    , y : Int
    , direction : Direction
    , isAlive : Bool
    }


type alias Bomb =
    { x : Int
    , y : Int
    , timer : Int
    }


type alias Point =
    { x : Int
    , y : Int
    }


toPoint item =
    { x = item.x, y = item.y }


type Direction
    = Left
    | Right
    | Up
    | Down
    | NoDirection


initialModel : Model
initialModel =
    { player =
        { x = halfSpriteWidth
        , y = halfSpriteHeight
        , direction = NoDirection
        , isAlive = True
        }
    , currentSprite = 0
    , bombs = []
    , explodingBombs = []
    , fires = []
    , bricks =
        [ { x = 2, y = 0 }
        , { x = 6, y = 0 }
        , { x = 4, y = 2 }
        , { x = 6, y = 2 }
        , { x = 10, y = 2 }
        , { x = 2, y = 4 }
        , { x = 4, y = 4 }
        , { x = 6, y = 4 }
        , { x = 2, y = 6 }
        , { x = 6, y = 6 }
        , { x = 8, y = 6 }
        , { x = 10, y = 6 }
        , { x = 2, y = 8 }
        , { x = 6, y = 8 }
        , { x = 8, y = 8 }
        , { x = 10, y = 8 }
        ]
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
                    |> updateFires
                    |> updateBombs timeDiff
                    |> maybeKillPlayer
                    |> updateSpriteNumber
                )

        KeyUp code ->
            case code of
                37 ->
                    noCmds { model | player = turnOff model.player Left }

                39 ->
                    noCmds { model | player = turnOff model.player Right }

                40 ->
                    noCmds { model | player = turnOff model.player Down }

                38 ->
                    noCmds { model | player = turnOff model.player Up }

                _ ->
                    noCmds model

        KeyDown code ->
            case code of
                32 ->
                    -- Space Bar --
                    dropBomb model

                37 ->
                    noCmds { model | player = turnOn model.player Left }

                39 ->
                    noCmds { model | player = turnOn model.player Right }

                40 ->
                    noCmds { model | player = turnOn model.player Down }

                38 ->
                    noCmds { model | player = turnOn model.player Up }

                _ ->
                    noCmds model


updateSpriteNumber : Model -> Model
updateSpriteNumber model =
    -- have to use spriteChangeRate parameter to slow down the rate that the sprite changes
    let
        numberOfSprites =
            3

        newSprite =
            if model.currentSprite >= (numberOfSprites * config.spriteChangeRate - 1) then
                -- reset back to 0
                0
            else
                -- increase the sprite model.currentSprite
                model.currentSprite + 1
    in
        { model | currentSprite = newSprite }


updateBombs : Time -> Model -> Model
updateBombs timeDiff model =
    let
        updateBombFunc =
            List.map
                (\bomb ->
                    { bomb
                        | timer = bomb.timer - (round timeDiff)
                    }
                )

        removeExpired =
            List.filter
                (\bomb ->
                    bomb.timer >= 0
                )

        updatedBombs =
            updateBombFunc model.bombs

        updatedExplodingBombs =
            updateBombFunc model.explodingBombs
    in
        { model
            | bombs = removeExpired updatedBombs
            , explodingBombs =
                (removeExpired updatedExplodingBombs)
                    ++ (updatedBombs
                            |> List.filter
                                (\bomb ->
                                    bomb.timer < 0
                                )
                            |> List.map
                                (\bomb ->
                                    { bomb | timer = 2000 }
                                )
                       )
        }


updateFires : Model -> Model
updateFires model =
    let
        notABrick point =
            not <| List.member point model.bricks

        fires =
            model.explodingBombs
                |> List.map toPoint
                |> List.concatMap
                    (\bomb ->
                        let
                            pointsAbove =
                                findPoints Up 3 bomb

                            pointsBelow =
                                findPoints Down 3 bomb

                            pointsRight =
                                findPoints Right 3 bomb

                            pointsLeft =
                                findPoints Left 3 bomb
                        in
                            ListEx.takeWhile notABrick pointsAbove
                                ++ ListEx.takeWhile notABrick pointsBelow
                                ++ ListEx.takeWhile notABrick pointsLeft
                                ++ ListEx.takeWhile notABrick pointsRight
                    )

        updatedBombs =
            model.bombs
                |> List.map
                    (\unexplodedBomb ->
                        if List.member { x = unexplodedBomb.x, y = unexplodedBomb.y } fires then
                            -- explode the bomb
                            { unexplodedBomb | timer = 0 }
                        else
                            unexplodedBomb
                    )
    in
        { model
            | fires = fires
            , bombs = updatedBombs
        }


move : Time -> Model -> Model
move timeDiff model =
    let
        player =
            model.player

        changeInPosition =
            timeDiff * config.velocity |> round

        ( currentCellX, currentCellY ) =
            positionToCell ( player.x, player.y )

        obstaclesInColumn =
            List.append (List.map toPoint model.bombs) model.bricks |> List.filter (\obs -> obs.x == currentCellX)

        obstaclesInRow =
            List.append (List.map toPoint model.bombs) model.bricks |> List.filter (\obs -> obs.y == currentCellY)
    in
        (case player.direction of
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
                        max leftBorder (player.x - changeInPosition)
                in
                    { player
                        | x = newX
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
                        min rightBorder (player.x + changeInPosition)
                in
                    { player
                        | x = newX
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
                        max topBorder (player.y - changeInPosition)
                in
                    { player
                        | y = newY
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
                        min botomBorder (player.y + changeInPosition)
                in
                    { player
                        | y = newY
                    }

            NoDirection ->
                player
        )
            |> (\player -> { model | player = player })


maybeKillPlayer : Model -> Model
maybeKillPlayer model =
    let
        player =
            model.player

        playerCell =
            uncurry Point <| positionToCell ( player.x, player.y )

        updatedPlayer =
            { player | isAlive = not (List.member playerCell model.fires) && player.isAlive}
    in
        { model | player = updatedPlayer }


turnOn : Player -> Direction -> Player
turnOn player direction =
    { player | direction = direction }


turnOff : Player -> Direction -> Player
turnOff player direction =
    if player.direction == direction then
        { player | direction = NoDirection }
    else
        player


dropBomb : Model -> ( Model, Cmd Msg )
dropBomb model =
    let
        ( cellX, cellY ) =
            positionToCell ( model.player.x, model.player.y )

        newBomb =
            { x = cellX, y = cellY, timer = 5000 }
    in
        noCmds { model | bombs = newBomb :: model.bombs }


findPoints : Direction -> Int -> Point -> List Point
findPoints direction limit point =
    case direction of
        Left ->
            List.range (point.x - limit) point.x
                |> List.map (\x -> { x = x, y = point.y })
                |> List.reverse

        Right ->
            List.range point.x (point.x + limit)
                |> List.map (\x -> { x = x, y = point.y })

        Up ->
            List.range (point.y - limit) point.y
                |> List.map (\y -> { y = y, x = point.x })
                |> List.reverse

        Down ->
            List.range point.y (point.y + limit)
                |> List.map (\y -> { y = y, x = point.x })

        _ ->
            [ { x = point.x, y = point.y } ]


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
            ++ (renderBombs model.currentSprite model.bombs)
            ++ (renderFires model.fires)
            ++ (renderBricks model.bricks)
            ++ [ renderPlayer model ]
        )


renderBricks : List Point -> List (Svg Msg)
renderBricks =
    let
        brickWidth =
            cellWidth |> toString

        brickHeight =
            cellHeight |> toString
    in
        List.map
            (\brick -> renderBrick brickColor brick.x brick.y)


renderBombs : Int -> List Bomb -> List (Svg Msg)
renderBombs currentSprite =
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
                        getBombSprite bomb currentSprite
                in
                    Svg.image [ Svg.xlinkHref spriteImage, Svg.width bombWidth, Svg.height bombHeight, Svg.x (toString bombX), Svg.y (toString bombY) ] []
            )


renderFires : List Point -> List (Svg Msg)
renderFires =
    let
        brickWidth =
            cellWidth |> toString

        brickHeight =
            cellHeight |> toString
    in
        List.map
            (\brick -> renderBrick fireColor brick.x brick.y)


renderPlayer : Model -> Svg Msg
renderPlayer model =
    let
        player =
            model.player

        playerWidth =
            cellWidth |> toString

        playerHeight =
            cellHeight |> toString

        centerX =
            (player.x - (cellWidth // 2)) |> toString

        centerY =
            (player.y - (cellHeight // 2)) |> toString

        spriteImage =
            getPlayerSprite model
    in
        if player.isAlive
        then
          Svg.image [ Svg.xlinkHref spriteImage, Svg.width playerWidth, Svg.height playerHeight, Svg.x centerX, Svg.y centerY ] []
        else
          Svg.text ""



grid : Model -> List (Svg Msg)
grid model =
    (List.map column <| List.range 0 config.numberOfColumns) |> List.concat


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
        "blue"
    else if cellNum % 2 == 0 then
        cellColor1
    else
        cellColor2


cellColor2 = "white"
cellColor1 = "rgb(230, 230, 230)"
fireColor = "yellow"
brickColor = "rgb(100, 50, 0)"

----- Assets -----------


getPlayerSprite : Model -> String
getPlayerSprite model =
    case model.player.direction of
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


getBombSprite : Bomb -> Int -> String
getBombSprite bomb currentSprite =
    let
        bombNumber =
            currentSprite // config.spriteChangeRate |> toString
    in
        assetPath ++ "bomb" ++ bombNumber ++ ".png"


assetPath : String
assetPath =
    "assets/"
