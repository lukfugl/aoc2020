module Password (checkSledPassword, checkTobogganPassword, Entry(..)) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex

data Entry = Entry Char Int Int String

instance Read Entry where
    readsPrec _ = readP_to_S $ do
            a <- readDecP
            b <- char '-' >> readDecP
            c <- skipSpaces >> lexChar
            s <- char ':' >> skipSpaces >> hsLex
            return $ Entry c a b s

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