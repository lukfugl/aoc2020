module Main where

import Inputs.Helpers (readAll)
import qualified Data.Map as M

seedMemory :: [Int] -> M.Map Int Int
seedMemory ns = foldr (\i -> M.insert (ns !! (i-1)) i) M.empty [1..length ns]

play :: [Int] -> Int -> Int
play start stop = go (seedMemory $ init start) (last start) (length start)
    where go mem k n
            | n == stop = k
            | otherwise =
              let k' = maybe 0 (n -) (M.lookup k mem)
                  mem' = M.insert k n mem
              in go mem' k' (n + 1)

readInput :: IO [Int]
readInput = readAll "days/Inputs/Day15.txt"

main :: IO ()
main = do
    input <- readInput
    putStrLn $ show $ play input 2020
    putStrLn $ show $ play input 30000000
    