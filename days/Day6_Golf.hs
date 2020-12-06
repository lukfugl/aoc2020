module Main where

import System.IO
import Data.List

countUnique :: [String] -> Int
countUnique = length . foldl union "" . sort

countShared :: [String] -> Int
countShared = length . foldl intersect "abcdefghijklmnopqrstuvwxyz" . sort

input :: IO [[String]]
input = openFile "days/Inputs/Day6.txt" ReadMode >>= gather [] []
    where gather acc1 acc2 handle = do
            done <- hIsEOF handle
            if done
                then return (acc2:acc1)
                else do
                    line <- hGetLine handle
                    if line == ""
                        then gather (acc2:acc1) [] handle
                        else gather acc1 (line:acc2) handle

main :: IO ()
main = do
    groups <- input
    putStrLn $ show $ sum $ map countUnique groups
    putStrLn $ show $ sum $ map countShared groups
