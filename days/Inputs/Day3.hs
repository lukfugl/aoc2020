module Inputs.Day3 (input) where

import System.IO

import Scenery (Map, MapLine)

parseLine :: String -> MapLine
parseLine = id

readNext :: Handle -> IO (Maybe MapLine)
readNext handle = do
    done <- hIsEOF handle
    if done
        then return Nothing
        else do
            line <- hGetLine handle
            return $ Just $ parseLine line

readRest :: Handle -> Map -> IO Map
readRest handle xs = do
    mx <- readNext handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO Map
input = do
    handle <- openFile "days/Inputs/Day3.txt" ReadMode
    fmap reverse $ readRest handle []
