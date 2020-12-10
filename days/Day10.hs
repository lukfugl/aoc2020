module Main where

import Inputs.Helpers (readAll)
import Data.List

readInput :: IO [Int]
readInput = readAll "days/Inputs/Day10.txt"

-- adapters parameter to both parts already includes the (max+3) device
-- adapter, but not the source (0 jolts)

-- part 1
gapProduct :: [Int] -> Int
gapProduct adapters =
    let diffs = map diff $ zip adapters (0:init adapters)
        a = length $ filter (== 1) diffs
        b = length $ filter (== 3) diffs
    in a * b
    where diff (n, m) = n - m

-- part 2
countArrangements :: [Int] -> Int
countArrangements adapters = snd $ head $ solve (0:adapters)
    where
        canReach a (b, _) = b - a <= 3
        solve [adapter] = [(adapter, 1)]
        solve (adapter:adapters) =
            let reachable = filter (canReach adapter) (solve adapters)
            in ((adapter, sum $ map snd reachable):reachable)

main :: IO ()
main = do
    input <- fmap sort readInput
    let adapters = input ++ [3 + last input]
    putStrLn $ show $ gapProduct adapters
    putStrLn $ show $ countArrangements adapters
