module Password (checkSledPassword, checkTobogganPassword, Entry(..)) where

import Data.Char (isDigit)

data Entry = Entry Char Int Int String
    deriving (Show)

instance Read Entry where
    readsPrec _ input =
        let (a',rest1) = span isDigit input
            a = read a' :: Int
            (_:rest2) = rest1 -- dash
            (b',rest3) = span isDigit rest2
            b = read b' :: Int
            (_:rest4) = rest3 -- space
            (c:rest5) = rest4
            (_:_:s) = rest5 -- ": <string>"
        in [(Entry c a b s, "")]

checkSledPassword :: Entry -> Bool
checkSledPassword (Entry c a b s) =
  let n = length $ filter (== c) s
  in a <= n && n <= b

checkTobogganPassword :: Entry -> Bool
checkTobogganPassword (Entry c a b s)
    | a < 1 || a > length s = False
    | b < 1 || b > length s = False
    | otherwise = 
        let a' = (s !! (a - 1)) == c
            b' = (s !! (b - 1)) == c
        in (a' || b') && not (a' && b')