module Main where

import qualified Inputs.Day2 (input)
import Password (checkSledPassword, checkTobogganPassword)

main :: IO ()
main = do
  input <- Inputs.Day2.input

  -- part 1
  putStrLn $ show $ length $ filter checkSledPassword input

  -- part 2
  putStrLn $ show $ length $ filter checkTobogganPassword input
