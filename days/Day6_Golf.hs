module Main where

import System.IO
import Data.List

-- should be able to reuse this for any "groups of lines separated by newline"
-- style input gathering
-- f is how to reduce the entries the groups of lines form
-- g is how to build up an entry from a group of lines
apply :: Show a => String -> (a -> b -> a) -> a -> (b -> String -> b) -> b -> IO ()
apply path f a0 g b0 = openFile path ReadMode >>= gatherRest a0 b0
    where gatherRest a b handle = do
            isEOF <- hIsEOF handle
            if isEOF
                then putStrLn $ show $ f a b
                else hGetLine handle >>= gatherOne a b handle
          gatherOne a b handle s = do
            if s == ""
                then gatherRest (f a b) b0 handle
                else gatherRest a (g b s) handle

main :: IO ()
main = do
    apply "days/Inputs/Day6.txt" (\n s -> n + length s) 0 union ""
    apply "days/Inputs/Day6.txt" (\n s -> n + length s) 0 intersect "abcdefghijklmnopqrstuvwxyz"
