module Scenery (Map, MapLine, countTrees) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex

type MapLine = String -- of # and .
type Map = [MapLine] -- of equal lengths

countTrees :: Int -> (Int, Int) -> Map -> Int
countTrees _ _ [] = 0
countTrees n (dx, dy) map =
    let line = head map
        dt = line !! n == '#'
        n' = (n + dx) `mod` (length line)
        map' = drop dy map
        t = countTrees n' (dx, dy) map'
    in if dt
        then t + 1
        else t
