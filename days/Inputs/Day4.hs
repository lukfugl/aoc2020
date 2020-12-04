module Inputs.Day4 (input) where

import System.IO

import Passport (Passport, readPassport)

readNext :: String -> Handle -> IO (Maybe Passport)
readNext s handle = do
    done <- hIsEOF handle
    if done
        then if s == ""
            then return Nothing
            else return $ Just $ readPassport s
        else do
            line <- hGetLine handle
            if line == ""
                then return $ Just $ readPassport s
                else readNext (s ++ " " ++ line) handle

readRest :: Handle -> [Passport] -> IO [Passport]
readRest handle xs = do
    mx <- readNext "" handle
    case mx of
        Nothing -> return xs
        Just x -> readRest handle (x:xs)

input :: IO [Passport]
input = do
    handle <- openFile "days/Inputs/Day4.txt" ReadMode
    fmap reverse $ readRest handle []
