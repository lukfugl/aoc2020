module BoardingPass (BoardingPass, readBoardingPass) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Text.Read hiding ((+++), choice, look)
import Data.Char

type BoardingPass = Int

seatP :: Int -> ReadP BoardingPass
seatP acc
    =   (choice [char 'B', char 'R'] >> seatP (acc * 2 + 1))
    +++ (choice [char 'L', char 'F'] >> seatP (acc * 2))
    +++ (eof >> return acc)

maybeParse :: String -> ReadP a -> Maybe a
maybeParse s p = case readP_to_S (p <* eof) s of
    [(a, _)] -> return a
    _ -> Nothing

readBoardingPass :: String -> BoardingPass
readBoardingPass s = case maybeParse s (seatP 0) of
    Just pass -> pass
    Nothing -> error $ "no parse: " ++ s
