module Inputs.Day2 (input) where

import System.IO

import Password (Entry(..))

parseLine :: String -> Entry
parseLine line = read line

readNext :: Handle -> IO (Maybe Entry)
readNext handle = do
    done <- hIsEOF handle
    if done
        then return Nothing
        else do
            line <- hGetLine handle
            return $ Just $ parseLine line

readRest :: Handle -> [Entry] -> IO [Entry]
readRest handle xs = do
    mx <- readNext handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO [Entry]
input = do
    handle <- openFile "days/Inputs/Day2.txt" ReadMode
    fmap reverse $ readRest handle []
