import Game
import Tetrominos

main = Html.App.program
    { init =
        (initialState, Cmd.none)
    , update = update
    , subscriptions model = case model of
        Menu m ->
            lnt
        Game g ->
            Sub.map Game Game.subscriptions
    , view = view
    }

type State
    = Menu
        { size: Int
        , dimensions: (Int, Int)
        }
    | Game Game.State

initial : State
initial = Menu {size = 4, dimensions = (7, 10)}

type Message
    = SizeChanged
    | Game Game.Message

update : Message -> State -> (State, Cmd)
update message state =
    case message of
        Game msg -> case State of
            Game s -> Game (Game.update msg s)
            _ ->
        SizeChanged ->

view model = case model of
    Menu m ->
        lnso
    Game g ->
        Game.view g
