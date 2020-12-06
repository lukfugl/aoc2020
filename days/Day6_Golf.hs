module Main where

import System.IO
import Data.List

apply :: (String -> String -> String) -> String -> IO ()
apply f id = openFile "days/Inputs/Day6.txt" ReadMode >>= gather 0 id
    where gather n s handle = do
            done <- hIsEOF handle
            if done
                then putStrLn $ show $ n + length s
                else do
                    line <- hGetLine handle
                    if line == ""
                        then gather (n + length s) id handle
                        else gather n (f line s) handle

main :: IO ()
main = do
    apply union ""
    apply intersect "abcdefghijklmnopqrstuvwxyz"
