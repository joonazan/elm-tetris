import Html exposing (..)
import Html.App
import Html.Events exposing (onClick)
import Game
import Tetrominos
import Array

main = Html.App.program
    { init =
        (initial, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions model = case model of
    Menu m ->
        Sub.none
    Game g ->
        Sub.map GameMsg Game.subscriptions

type State
    = Menu Model
    | Game Game.Model

type alias Model =
    { size: Int
    , width: Int
    , height: Int
    }

initial : State
initial = Menu {size = 4, width = 7, height = 10}

type Message
    = MenuMsg MenuMessage
    | GameMsg Game.Message

type MenuMessage
    = StartGame
    | Change (Model -> Model)

update : Message -> State -> (State, Cmd Message)
update message state =
    case (message, state) of
        (GameMsg msg, Game s) ->
            let (state, cmd) = (Game.update msg s)
            in (Game state, Cmd.map GameMsg cmd)
        (MenuMsg msg, Menu s) ->
            updateMenu msg s
        _ -> (state, Cmd.none)

updateMenu message state = case message of
    StartGame ->
        (Game <| Game.initial (Tetrominos.tetrominos state.size) (state.width, state.height), Cmd.none)
    Change f -> (Menu <| f state, Cmd.none)

view model = case model of
    Menu m ->
        div[]
            [ h1 [] [text "Putoava palikkapeli"]
            , p []
                [ text ("mino size: " ++ toString m.size)
                , button[onClick (MenuMsg <| Change increaseSize)][text "moar!"]
                , br[][]

                , text ("number of different minoes: " ++
                    (toString <| Array.length <| Tetrominos.tetrominos m.size))
                , br[][]

                , button[onClick <| MenuMsg <| Change (\m -> {m | width = m.width-1})] [text "-"]
                , text <| toString m.width
                , button[onClick <| MenuMsg <| Change (\m -> {m | width = m.width+1})] [text "+"]
                , br[][]

                , button[onClick <| MenuMsg <| Change (\m -> {m | height = m.height-1})] [text "-"]
                , text <| toString m.height
                , button[onClick <| MenuMsg <| Change (\m -> {m | height = m.height+1})] [text "+"]
                , br[][]
                ]
            , button[onClick (MenuMsg StartGame)][text "Start"]
            ]
    Game g ->
        Game.view g

increaseSize model =
    {model | size = model.size+1}
