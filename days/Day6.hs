module Main where

import qualified Inputs.Day6 (input)
import AnswerSheet (GroupAnswers)
import Data.List

countUnique :: GroupAnswers -> Int
countUnique = length . foldl union ""

countShared :: GroupAnswers -> Int
countShared = length . foldl intersect "abcdefghijklmnopqrstuvwxyz"

main :: IO ()
main = do
    groups <- Inputs.Day6.input
    putStrLn $ show $ sum $ map countUnique groups
    putStrLn $ show $ sum $ map countShared groups
