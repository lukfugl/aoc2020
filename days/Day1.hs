module Main where

import Expense (findEntryPair, findEntryTriple)
import Inputs.Day1 (input)

main :: IO ()
main = do
  -- part1
  let (a, b) = findEntryPair 2020 input
  putStrLn $ show (a * b)

  -- part2
  let (a', b', c) = findEntryTriple 2020 input
  putStrLn $ show (a' * b' * c)
