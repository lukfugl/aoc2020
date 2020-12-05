module Inputs.Day5 (input) where

import System.IO

import BoardingPass (BoardingPass, readBoardingPass)

parseLine :: String -> BoardingPass
parseLine = readBoardingPass

readNext :: Handle -> IO (Maybe BoardingPass)
readNext handle = do
    done <- hIsEOF handle
    if done
        then return Nothing
        else do
            line <- hGetLine handle
            return $ Just $ parseLine line

readRest :: Handle -> [BoardingPass] -> IO [BoardingPass]
readRest handle xs = do
    mx <- readNext handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO [BoardingPass]
input = do
    handle <- openFile "days/Inputs/Day5.txt" ReadMode
    fmap reverse $ readRest handle []
