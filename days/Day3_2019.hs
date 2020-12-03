module Main where

import Wiring (distanceToClosestIntersection)
import qualified Inputs.Day3_2019 (input)

main :: IO ()
main = do
  wires <- Inputs.Day3_2019.input
  let solution1 = distanceToClosestIntersection (wires !! 0) (wires !! 1)
  putStrLn $ show solution1
