module Main where

import Inputs.Helpers (loadStringGroups)
import Data.List

main :: IO ()
main = do
    groups <- loadStringGroups "days/Inputs/Day6.txt"
    putStrLn $ show $ sum $ map (length . foldl union "") groups
    putStrLn $ show $ sum $ map (length . foldl intersect "abcdefghijklmnopqrstuvwxyz") groups
