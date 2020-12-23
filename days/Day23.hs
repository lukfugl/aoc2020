module Main where

import Prelude hiding (lookup)
import Data.List (find)
import Data.HashTable.IO (BasicHashTable, fromList, lookup, insert)
import Data.Maybe (fromJust)

type Links = BasicHashTable Int Int
data Circle = Circle Int Int Links

asCircle :: [Int] -> IO Circle
asCircle ns = do
    let offset = take (length ns) $ tail $ cycle ns
    links <- fromList $ zip ns offset
    return $ Circle (head ns) (length ns) links

lookupLink :: Links -> Int -> IO Int
lookupLink links = fmap fromJust . lookup links

example :: [Int]
example = [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: [Int]
input = [3, 6, 4, 2, 8, 9, 7, 1, 5]

unwindN :: Links -> Int -> Int -> IO [Int]
unwindN _ _ 0 = return []
unwindN links k n = do
    k' <- lookupLink links k
    fmap (k:) $ unwindN links k' (n-1)

unwind :: Int -> Circle -> IO [Int]
unwind k (Circle _ n links) = unwindN links k n

unwind' :: Circle -> IO [Int]
unwind' circle@(Circle k _ _) = unwind k circle

move :: Circle -> IO Circle
move (Circle current n links) = do
    next4 <- fmap (drop 1) $ unwindN links current 5
    let pickedUp = take 3 next4
        next = last next4
        destination = fromJust $ find (`notElem` pickedUp) $ map (\d -> ((current - d - 1) `mod` n) + 1) [1..5]
    afterDestination <- lookupLink links destination
    insert links destination (head pickedUp)
    insert links (last pickedUp) afterDestination
    insert links current next
    return $ Circle next n links

moveN :: Int -> Circle -> IO Circle
moveN 0 cups = return cups
moveN n cups = do
    cups' <- move cups
    moveN (n - 1) cups'

part1 :: Int -> [Int] -> IO [Int]
part1 n cups = do
    initial <- asCircle cups
    final <- moveN n initial
    list <- unwind 1 final
    return $ tail list

part2 :: Int -> Int -> [Int] -> IO Int
part2 n m cups = do
    let cups' = cups ++ drop (length cups) [1..n]
    answer <- fmap (take 2) $ part1 m cups'
    let a = head answer
    let b = head $ tail answer
    return $ a * b

main :: IO ()
main = do
    part1' <- part1 100 input
    putStrLn $ show part1'
    part2' <- part2 1000000 10000000 input
    putStrLn $ show part2'
