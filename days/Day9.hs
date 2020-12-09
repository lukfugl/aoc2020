module Main where

import Inputs.Helpers (readAll)
import Data.List

asCandidates :: [Int] -> [[Int]]
asCandidates preamble =
    let indices = [0..length preamble-1]
    in map (\i -> let a = preamble !! i
                  in map (\j -> a + (preamble !! j)) $ filter (/= i) indices) indices

valid :: [[Int]] -> Int -> Bool
valid candidates n = any (n `elem`) candidates

validate :: [[Int]] -> [Int] -> [Int] -> Int
validate candidates currents [] = error "reached end"
validate candidates currents (n:rest) =
    if not $ valid candidates n
        then n
        else let newCandidates = map (+ n) $ tail currents
                 candidates' = tail candidates ++ [newCandidates]
                 currents' = tail currents ++ [n]
             in validate candidates' currents' rest

findRegion :: [Int] -> Int -> Int
findRegion input target =
    let sublists = concatMap (\lst -> filter (\sublst -> length sublst >= 2) $ tails lst) $ filter (\lst -> length lst >= 2) $ inits input
    in case find (\lst -> sum lst == target) sublists of
        Nothing -> error "not found"
        Just sublst ->
            let sorted = sort sublst
            in (head sorted) + (last sorted)

main :: IO ()
main = do
    input <- readAll "days/Inputs/Day9.txt"
    let preamble = take 25 input
    let input' = drop 25 input
    let candidates = asCandidates preamble
    let sol1 = validate candidates preamble input'
    putStrLn $ show $ sol1
    putStrLn $ show $ findRegion input sol1
