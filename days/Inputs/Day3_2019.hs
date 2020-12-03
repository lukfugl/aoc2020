module Inputs.Day3_2019 (input) where

import System.IO

import Wiring (Wire, readWire)

parseLine :: String -> Wire
parseLine = readWire

readNext :: Handle -> IO (Maybe Wire)
readNext handle = do
    done <- hIsEOF handle
    if done
        then return Nothing
        else do
            line <- hGetLine handle
            return $ Just $ parseLine line

readRest :: Handle -> [Wire] -> IO [Wire]
readRest handle xs = do
    mx <- readNext handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO [Wire]
input = do
    handle <- openFile "days/Inputs/Day3_2019.txt" ReadMode
    fmap reverse $ readRest handle []
