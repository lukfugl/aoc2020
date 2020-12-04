module Main where

import qualified Inputs.Day4 (input)
import Passport (semiValidPassport, validPassport)

main :: IO ()
main = do
    passports <- Inputs.Day4.input
    putStrLn $ show $ length $ filter semiValidPassport passports
    putStrLn $ show $ length $ filter validPassport passports
