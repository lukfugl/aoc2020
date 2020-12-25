module Main where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

rot :: Int -> Int -> Int
rot subject value = (subject * value) `mod` 20201227

rotN :: Int -> Int -> Int
rotN subject 0 = 1
rotN subject n = rot subject $ rotN subject (n - 1)

pubKeys :: [Int]
pubKeys = iterate (rot 7) 1

main = do
    let cardPubKey = 6270530
    let doorPubKey = 14540258
    let cardLoopSize = fromJust $ elemIndex cardPubKey pubKeys
    let doorLoopSize = fromJust $ elemIndex doorPubKey pubKeys
    let encryptionKey = rotN doorPubKey cardLoopSize
    putStrLn $ show encryptionKey