module Tetrominos exposing (tetrominos, rotate)

import Array
import Position
import Set

size = 4

tetrominos =
    Array.fromList <| Set.toList <| Set.fromList (List.map canonicalize (allVariations [(0, 0)]))

adjacent (x, y) =
    [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]

allVariations partial =
    let
        oneLevel =
            let
                adjacents = (List.concatMap adjacent partial)
                free = List.filter (\x -> not <| List.member x partial) adjacents
            in
                List.map (\x -> x :: partial) free
    in
        if (List.length partial) == size then
            [partial]
        else
            List.concatMap allVariations oneLevel

-- Transforms all arbitrarily rotated and translated versions of a tetromino into the same version
canonicalize tetromino =
    let
        centered = center tetromino
        orientations =
            List.map (\x -> rotate x centered) [0..4]
        cmp (x1, y1) (x2, y2) =
            if x1 > x2 || (x1 == x2 && y1 > y2) then
                GT
            else if x1 == x2 && y1 == y2 then
                EQ
            else
                LT
        sorted = List.map (List.sortWith cmp) orientations
        first t1 t2 =
            let smaller = List.foldl (\new verdict ->
                    if verdict == EQ then new else verdict) EQ (List.map2 cmp t1 t2)
            in if smaller == GT then t1 else t2
    in
        case sorted of
            head :: tail ->
                List.foldl first head tail
            [] -> []

center tetromino =
    let
        (x, y) = List.foldl Position.add (0, 0) tetromino
        offset = (round (-x/size), round (-y/size))
    in
        List.map (Position.add offset) tetromino

rotate amount tetromino =
    let
        applyN f times x =
            List.foldl (\n o -> f o) x [0..times]
    in
        applyN rotateCw amount tetromino

rotateCw tetromino =
    List.map (\(x, y) -> (y, -x)) tetromino