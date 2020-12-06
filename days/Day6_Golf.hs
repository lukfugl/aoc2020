module Main where

import System.IO
import Data.List

-- should be able to just get form Control.Monad.Extra, but that's refusing to
-- build on my WSL setup. counting towards lines as if imported :p
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c a b = c >>= (\c' -> if c' then a else b)

apply :: (String -> String -> String) -> String -> IO ()
apply f id = openFile "days/Inputs/Day6.txt" ReadMode >>= gatherRest 0 id
    where gatherRest n s handle = do
            ifM (hIsEOF handle)
                (putStrLn $ show $ n + length s)
                (hGetLine handle >>= gatherOne n s handle)
          gatherOne n s handle line = do
            if line == ""
                then gatherRest (n + length s) id handle
                else gatherRest n (f line s) handle

main :: IO ()
main = do
    apply union ""
    apply intersect "abcdefghijklmnopqrstuvwxyz"
