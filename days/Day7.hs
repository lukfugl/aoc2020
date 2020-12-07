module Main where

import Inputs.Helpers (loadStrings)
import Luggage (merge, reverse, reachable, contained)
import Prelude hiding (reverse)

main :: IO ()
main = do
    graph <- fmap (merge . map read) $ loadStrings "days/Inputs/Day7.txt"
    putStrLn $ show $ (length $ reachable "shiny gold" $ reverse graph) - 1
    putStrLn $ show $ contained "shiny gold" graph
    