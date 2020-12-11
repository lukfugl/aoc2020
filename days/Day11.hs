module Main where

import Inputs.Helpers (loadStrings)

type Map = [String]

readInput :: IO Map
readInput = loadStrings "days/Inputs/Day11.txt"

directions :: [(Int, Int)]
directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

countNeighbors :: Map -> Int -> Int -> Int
countNeighbors seats x y = length $ filter (check . move) directions
    where move (dx, dy) = (x + dx, y + dy)
          inBounds (x', y')
                | y' < 0 || y' >= length seats = False
                | x' < 0 || x' >= length (seats !! y') = False
                | otherwise = True
          isOccupied (x', y') = (seats !! y') !! x' == '#'
          check loc = inBounds loc && isOccupied loc

countExtendedNeighbors :: Map -> Int -> Int -> Int
countExtendedNeighbors seats x y = length $ filter (isFirstOccupied . lineOfSight) directions
    where lineOfSight dir = takeWhile inBounds $ map (move dir) [1..]
          move (dx, dy) n = (x + n * dx, y + n * dy)
          inBounds (x', y')
                | y' < 0 || y' >= length seats = False
                | x' < 0 || x' >= length (seats !! y') = False
                | otherwise = True
          at (x', y') = (seats !! y') !! x'
          isSeat (x', y') = (seats !! y') !! x' == '#'
          isFirstOccupied [] = False
          isFirstOccupied (loc:rest) =
                case at loc of
                    '#' -> True
                    'L' -> False
                    _ -> isFirstOccupied rest

runGeneration :: Map -> Map
runGeneration seats =
    flip map [0..(length seats) - 1] $ \y ->
        flip map [0..(length $ seats !! y) - 1] $ \x ->
            let ch = (seats !! y) !! x
            in if ch == '.'
                then '.'
                else let n = countNeighbors seats x y
                     in if n == 0
                         then '#'
                         else if n >= 4
                             then 'L'
                             else ch

runGeneration' :: Map -> Map
runGeneration' seats =
    flip map [0..(length seats) - 1] $ \y ->
        flip map [0..(length $ seats !! y) - 1] $ \x ->
            let ch = (seats !! y) !! x
            in if ch == '.'
                then '.'
                else let n = countExtendedNeighbors seats x y
                     in if n == 0
                         then '#'
                         else if n >= 5
                             then 'L'
                             else ch

runToStability :: Map -> Map
runToStability seats =
    let seats' = runGeneration seats
    in if seats' == seats
        then seats'
        else runToStability seats'

runToStability' :: Map -> Map
runToStability' seats =
    let seats' = runGeneration' seats
    in if seats' == seats
        then seats'
        else runToStability' seats'

countOccupied :: Map -> Int
countOccupied seats = sum $ map (length . filter (== '#')) seats

main :: IO ()
main = do
    seats <- readInput
    putStrLn $ show $ countOccupied $ runToStability seats
    putStrLn $ show $ countOccupied $ runToStability' seats
