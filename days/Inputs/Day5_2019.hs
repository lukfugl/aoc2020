module Inputs.Day5_2019 (input) where

import System.IO

input :: IO String
input = openFile "days/Inputs/Day5_2019.txt" ReadMode >>= hGetLine
