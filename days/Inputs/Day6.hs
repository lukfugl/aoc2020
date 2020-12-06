module Inputs.Day6 (input) where

import System.IO
import Data.List

import AnswerSheet (AnswerSheet, GroupAnswers)

readNext :: GroupAnswers -> Handle -> IO GroupAnswers
readNext answers handle = do
    done <- hIsEOF handle
    if done
        then return answers
        else do
            line <- hGetLine handle
            if line == ""
                then return answers
                else readNext (sort line:answers) handle

readRest :: Handle -> [GroupAnswers] -> IO [GroupAnswers]
readRest handle xs = do
    x <- readNext [] handle
    case x of
        [] -> return xs
        _ -> readRest handle (x:xs)

input :: IO [GroupAnswers]
input = do
    handle <- openFile "days/Inputs/Day6.txt" ReadMode
    fmap reverse $ readRest handle []
