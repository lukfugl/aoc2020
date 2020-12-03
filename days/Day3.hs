module Main where

import qualified Inputs.Day3 (input)
import Scenery (countTrees)

main :: IO ()
main = do
    map <- Inputs.Day3.input

    -- part 1
    putStrLn $ show $ countTrees 0 (3, 1) map

    -- part 2
    let slopes = [ (1, 1), (3, 1), (5, 1), (7, 1), (1, 2) ]
    let treeCounts = fmap (\slope -> countTrees 0 slope map) slopes
    putStrLn $ show $ product treeCounts
