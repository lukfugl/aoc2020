module Main where

import qualified Data.Set as Set
import Control.Monad.State.Lazy

example :: [Int]
example = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [3, 6, 4, 2, 8, 9, 7, 1, 5]

findCycleM :: Int -> State ([Int], Set.Set [Int]) Int
findCycleM n = do
    (cups, seen) <- get
    if Set.member cups seen
        then return n
        else do
            let cups' = move cups
            let seen' = Set.insert cups seen
            put (cups', seen')
            findCycleM (n + 1)

findCycle :: [Int] -> Int
findCycle cups = evalState (findCycleM 0) (cups, Set.empty)

move :: [Int] -> [Int]
move (cup:cups) =
    let pickedUp = take 3 cups
        remaining = drop 3 cups
        below = filter (< cup) remaining
        candidates = if below == [] then remaining else below
        destination = maximum candidates
        beforeDestination = takeWhile (/= destination) remaining
        (_:afterDestination) = dropWhile (/= destination) remaining
    in beforeDestination ++ (destination:pickedUp) ++ afterDestination ++ [cup]

moveN :: Int -> [Int] -> [Int]
moveN 0 cups = cups
moveN n cups = moveN (n - 1) $ move cups

part1 :: Int -> [Int] -> [Int]
part1 n cups =
    let final = moveN n cups
        before1 = takeWhile (/= 1) final
        after1 = tail $ dropWhile (/= 1) final
    in after1 ++ before1

extend :: Int -> [Int] -> [Int]
extend n cups = cups ++ drop (length cups) [1..n]

part2 :: Int -> [Int] -> (Int, Int)
part2 n m cups =
    let cups' = extend n cups
        k = findCycle cups'
        m' = m `mod` k
        (a:b:_) = part1 n' cups'
    in (a, b)

main :: IO ()
main = do
    --putStrLn $ show $ part1 100 input
    let answer = findCycle $ extend 1000000 example
    putStrLn $ show answer
