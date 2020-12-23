module Main where

import Data.List (find)
import Data.Map.Strict as Map (Map, fromList, (!), insert)
import Data.Maybe (fromJust)

data Circle = Circle Int Int (Map Int Int)

asCircle :: [Int] -> Circle
asCircle ns =
    let offset = take (length ns) $ drop 1 $ cycle ns
    in Circle (head ns) (length ns) (fromList $ zip ns offset)

example :: Circle
example = asCircle [3, 8, 9, 1, 2, 5, 4, 6, 7]

input :: Circle
input = asCircle [3, 6, 4, 2, 8, 9, 7, 1, 5]

unwindN :: Map Int Int -> Int -> Int -> [Int]
unwindN _ _ 0 = []
unwindN links k n = (k:unwindN links (links ! k) (n-1))

unwind :: Int -> Circle -> [Int]
unwind k (Circle _ n links) = unwindN links k n

unwind' :: Circle -> [Int]
unwind' circle@(Circle k _ _) = unwind k circle

instance Show Circle where
    show circle =
        let list = unwind' circle
        in show' list
        where show' [a] = show a
              show' (a:as) = show a ++ " " ++ show' as

extend :: Int -> Circle -> Circle
extend n circle =
    let list = unwind' circle
        list' = list ++ drop (length list) [1..n]
    in asCircle list'

move :: Circle -> Circle
move (Circle current n links) =
    let next4 = drop 1 $ unwindN links current 5
        pickedUp = take 3 next4
        next = last next4
        destination = fromJust $ find (`notElem` pickedUp) $ map (\d -> ((current - d - 1) `mod` n) + 1) [1..5]
        links' = insert destination (head pickedUp) $ insert (last pickedUp) (links ! destination) $ insert current next links
    in Circle next n links'

moveN :: Int -> Circle -> Circle
moveN 0 cups = cups
moveN n cups = moveN (n - 1) $ move cups

part1 :: Int -> Circle -> [Int]
part1 n cups = drop 1 $ unwind 1 $ moveN n cups

main :: IO ()
main = do
    putStrLn $ show $ part1 100 input
    putStrLn $ show $ take 2 $ part1 10000000 $ extend 1000000 input
