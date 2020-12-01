module Expense (findEntryPair, findEntryTriple) where

import Data.List
import Data.Maybe

pairEntries' :: Int -> [Int] -> Maybe (Int, Int)
pairEntries' _ [] = Nothing
pairEntries' _ [a] = Nothing
pairEntries' n lst
    | a + b == n = return (a, b)
    | a + b > n = pairEntries' n $ init lst
    | otherwise = pairEntries' n $ tail lst
    where a = head lst
          b = last lst

tripleEntries' :: Int -> [Int] -> Maybe (Int, Int, Int)
tripleEntries' n [] = Nothing
tripleEntries' n (a:as)
    | a * 3 > n = Nothing
    | otherwise = case pairEntries' (n - a) as of
                    Just (b, c) -> return (a, b, c)
                    Nothing -> tripleEntries' n as


findEntryPair :: Int -> [Int] -> (Int, Int)
findEntryPair n lst = fromJust $ pairEntries' n $ sort lst

findEntryTriple :: Int -> [Int] -> (Int, Int, Int)
findEntryTriple n lst = fromJust $ tripleEntries' n $ sort lst
