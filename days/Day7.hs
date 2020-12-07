module Main where

import Inputs.Helpers (loadStrings)
import Luggage (LuggageRule, asGraph, transitiveClosure)
import Data.List
import Text.Read (readMaybe)

asVertexPair vertices n e =
    let (i, j) = e `divMod` n
        i' = vertices !! i
        j' = vertices !! j
    in show (i', j')

main :: IO ()
main = do
    rules <- fmap (map read) $ loadStrings "days/Inputs/Day7.txt"
    let graph = asGraph rules
    let (vertices, n, edges) = transitiveClosure graph
    sequence $ map (putStrLn . asVertexPair vertices n) edges
    return ()
