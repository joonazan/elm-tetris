module KeysDown exposing (Model, keyPressed, frameEnded, Message, initial, update, subscriptions)

import Keyboard
import Dict

type alias Model =
    { pressedKeys : KeyDict
    , previousKeys : KeyDict
    , tappedKeys : KeyDict
    }
type alias KeyDict = Dict.Dict Keyboard.KeyCode Bool

keyPressed key model =
    (getKey key model.pressedKeys) || (getKey key model.tappedKeys)

initial : Model
initial =
    { pressedKeys = Dict.empty
    , previousKeys = Dict.empty
    , tappedKeys = Dict.empty
    }

type Message
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode

update message model =
    case message of
        KeyDown key ->
            { model | pressedKeys = Dict.insert key True model.pressedKeys }
        KeyUp key ->
            -- Sticky keys: If a key is tapped, it should appear pressed for one frame
            let
                tapped = not (getKey key model.previousKeys)
            in 
                { model | pressedKeys = Dict.insert key False model.pressedKeys
                , tappedKeys = Dict.insert key tapped model.tappedKeys
                }

frameEnded model =
    { model | previousKeys = model.pressedKeys
    , tappedKeys = Dict.empty
    }

getKey key keys = Maybe.withDefault False (Dict.get key keys)

subscriptions convert =
    Sub.batch
        [ Keyboard.downs (\x -> convert (KeyDown x))
        , Keyboard.ups (\x -> convert (KeyUp x))
        ]