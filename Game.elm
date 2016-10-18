module Game exposing (subscriptions, initial, update, view)

import Html
import Html.App
import Collage
import Element
import Color
import AnimationFrame
import KeysDown
import Array
import Grid
import Position
import Random
import Tetrominos

subscriptions = Sub.batch
    [ AnimationFrame.diffs TimePassed
    , KeysDown.subscriptions KeysDownMsg
    ]

spawnPos (width, height) = (width // 2, height)

initial shapes dimensions =
    { shapes = shapes
    , dimensions = dimensions
    , time = 0
    , fallingTetromino = 
        { position = spawnPos
        , orientation = 0
        , shape = 0
        , lastMoved = 0
        , lastFell = 0
        }
    , spawned = 0
    , keysDown = KeysDown.initial
    , grid = Grid.new dimensions
    , gameOver = False
    }

type Message
    = TimePassed Float
    | SpawnTetromino Int
    | KeysDownMsg KeysDown.Message

update message model =
    case message of
        TimePassed millis ->
            updateGame { model | time = model.time + millis }

        SpawnTetromino shape ->
            let ft = model.fallingTetromino
            in noEffect { model | fallingTetromino =
                { ft | position = spawnPos
                , orientation = 0
                , shape = shape
                }
                , spawned = model.spawned + 1
            }

        KeysDownMsg msg ->
            noEffect { model | keysDown = KeysDown.update msg model.keysDown }

noEffect model = (model, Cmd.none)

left = 37
right = 39
up = 38
down = 40

updateGame model =
    let
        keyPressed key =
            KeysDown.keyPressed key model.keysDown
        (movedTetromino, hitGround) =
            move model.fallingTetromino model.time keyPressed model.grid
        newModel =
            { model | keysDown = KeysDown.frameEnded model.keysDown }
        (newGrid, gameOver) =
            writeTetromino movedTetromino model.grid
    in
        if hitGround then
            ( { newModel | grid = newGrid, gameOver = gameOver }
            , Random.generate SpawnTetromino (Random.int 0 (Array.length model.shapes - 1)))
        else
            noEffect { newModel | fallingTetromino = movedTetromino }

writeTetromino tetromino grid =
    let
        positions = (absolutePositions tetromino)
        gameOver =
            List.any (\(x, y) -> y >= Grid.height grid) positions
    in
        (Grid.removeRows <| List.foldl Grid.set grid positions, gameOver)

-- TODO: Low framerates can make the block move slightly slower. Find a way to prevent that.
moveCooldown = 100
dropCooldown = 1000

move tetromino time keyPressed grid =
    let
        translate offset tetromino =
            { tetromino | position = Position.add tetromino.position offset }

        nonoverlapping tetromino =
            not (collides tetromino grid)
        
        rotate tetromino =
            let
                rotated =
                    { tetromino | orientation = (tetromino.orientation + 1) % 4 }
                rotateds =
                    [ rotated
                    , translate (1, 0) rotated
                    , translate (-1, 0) rotated
                    ]
                possible = List.filter nonoverlapping rotateds
            in
                if time - tetromino.lastMoved > moveCooldown && keyPressed up then
                    case possible of
                        head :: _ -> { head | lastMoved = time }
                        [] -> tetromino
                else
                    tetromino

        move tetromino =
            let
                strafeDirection =
                    (if keyPressed left then -1 else 0) + (if keyPressed right then 1 else 0)
                translated = translate (strafeDirection, 0) tetromino
                moved =
                    { translated | lastMoved = time }
            in
                if time - tetromino.lastMoved > moveCooldown
                    && strafeDirection /= 0
                    && nonoverlapping moved then
                    moved
                else
                    tetromino

        drop tetromino =
            let
                dt = time - tetromino.lastFell
                fallen = translate (0, -1) tetromino
            in
                if dt > dropCooldown || (dt > moveCooldown && keyPressed down) then
                    if collides fallen grid then
                        (tetromino, True)
                    else
                        ({ fallen | lastFell = time}, False)
                else
                    (tetromino, False)
    in
        drop <| move <| rotate tetromino

collides shapes grid tetromino =
    let
        positions = absolutePositions tetromino shapes
        collides (x, y) =
            y < 0 || x < 0 || x >= Grid.width grid || (Grid.at (x, y) grid)
    in
        List.any collides positions

absolutePositions shapes tetromino =
    let
        shape =
            Maybe.withDefault [] (Array.get tetromino.shape shapes)
        rotated = Tetrominos.rotate tetromino.orientation shape
    in
        List.map (Position.add tetromino.position) rotated

view model =
    let
        screenWidth = (Grid.width model.grid)*boxSize
        screenHeight = (Grid.height model.grid)*boxSize

        draw shapes = Element.toHtml <| Collage.collage screenWidth screenHeight shapes

        drawFalling tetromino =
            List.map box (absolutePositions model.shapes tetromino)

        box : (Int, Int) -> Collage.Form
        box (x, y) =
            let
                blackSquare = Collage.filled black (Collage.rect boxSize boxSize)
                screenPos =
                    ( toFloat x*boxSize - (screenWidth/2) + (boxSize/2)
                    , toFloat y*boxSize - (screenHeight/2) + (boxSize/2)
                    )
            in
                Collage.move screenPos blackSquare
        
        boxSize = 25
        black = Color.rgb 0 0 0
    in
        Html.div []
        [ Html.text <| status model
        , draw (Collage.filled (Color.rgb 200 200 200) (Collage.rect screenWidth screenHeight)
            :: (drawFalling model.fallingTetromino)
            ++ (List.map box (Grid.positions model.grid)))
        ]

status model =
    if model.gameOver then
        "Game over."
    else
        "Minoes survived: " ++ (toString model.spawned)
