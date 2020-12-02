module Inputs.Day1 (input) where

import System.IO

readNext :: Handle -> IO (Maybe Int)
readNext handle = do
    done <- hIsEOF handle
    if done
        then return Nothing
        else do
            line <- hGetLine handle
            return $ Just $ read line

readRest :: Handle -> [Int] -> IO [Int]
readRest handle xs = do
    mx <- readNext handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO [Int]
input = do
    handle <- openFile "days/Inputs/Day1.txt" ReadMode
    fmap reverse $ readRest handle []
