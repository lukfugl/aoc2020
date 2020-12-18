module Main where

import Inputs.Helpers (loadStrings)

import Text.ParserCombinators.ReadP
import Text.Read.Lex

-- shared
plusP :: ReadP (Int -> Int -> Int)
plusP = string " + " >> return (+)

timesP :: ReadP (Int -> Int -> Int)
timesP = string " * " >> return (*)

unitP :: ReadP Int -> ReadP Int
unitP p = choice [ readDecP, between (char '(') (char ')') p ]

-- part 1
part1 :: ReadP Int
part1 = chainl1 (unitP part1) (plusP +++ timesP)

data Part1 = Part1 Int
instance Read Part1 where
    readsPrec _ = readP_to_S $ fmap Part1 part1 <* eof

reduce1 :: String -> Int
reduce1 s = let (Part1 n) = read s in n

-- part 2
factorP :: ReadP Int
factorP = sumP <++ unitP part2

termP :: ReadP Int
termP = unitP part2

productP :: ReadP Int
productP = chainl1 factorP timesP
        
sumP :: ReadP Int
sumP = chainl1 termP plusP

part2 :: ReadP Int
part2 = productP
    
data Part2 = Part2 Int
instance Read Part2 where
    readsPrec _ = readP_to_S $ fmap Part2 part2 <* eof

reduce2 :: String -> Int
reduce2 s = let (Part2 n) = read s in n

-- driver
main :: IO ()
main = do
    input <- loadStrings "days/Inputs/Day18.txt"
    putStrLn $ show $ sum $ map reduce1 input
    putStrLn $ show $ sum $ map reduce2 input
