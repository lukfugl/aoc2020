module Main where

import qualified Inputs.Day5 (input)
import BoardingPass (BoardingPass)

import Data.List

findGap :: [BoardingPass] -> BoardingPass
findGap (a:b:_) | b == a + 2 = a + 1
findGap (_:rest) = findGap rest

main :: IO ()
main = do
    passes <- fmap sort Inputs.Day5.input
    putStrLn $ show $ last passes
    putStrLn $ show $ findGap passes