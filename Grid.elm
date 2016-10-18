module Grid exposing (..)

import Array

type alias Grid = Array.Array (Array.Array Bool)

new width height =
    Array.initialize height (always (Array.initialize width (always False)))

at (x, y) grid =
    Maybe.withDefault False (Maybe.andThen (Array.get y grid) (Array.get x))

set (x, y) grid =
    let
        row = Maybe.withDefault Array.empty (Array.get y grid)
    in
        Array.set y (Array.set x True row) grid

removeRows grid =
    let
        all f =
            Array.foldl (\a sum -> (f a) && sum) True
        withoutRows = 
            Array.filter (\a -> not <| all identity a) grid
        removed = (height grid) - (height withoutRows)
     in
        Array.append withoutRows (new (width grid) removed)

positions grid =
    let
        coordinates =
            List.concatMap (\x -> List.map (\y -> (x, y)) [0..height grid]) [0..width grid]
    in
        List.filter (\pos -> at pos grid) coordinates

height grid =
    Array.length grid
width grid =
    Array.length (Maybe.withDefault Array.empty (Array.get 0 grid))