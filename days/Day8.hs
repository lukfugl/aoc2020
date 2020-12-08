module Main where

import Inputs.Helpers (readAll)
import GameCode (Program, Instruction(..), Op(..), runProgram)
import Data.Either

mutate :: Instruction -> Maybe Instruction
mutate (Instruction Acc n) = Nothing
mutate (Instruction Jmp n) = return $ Instruction Nop n
mutate (Instruction Nop n) = return $ Instruction Jmp n

mutations :: Program -> [Program]
mutations [] = []
mutations (i:rest) =
    case mutate i of
        Nothing -> map (i:) (mutations rest)
        Just i' -> ((i':rest):map (i:) (mutations rest))

findCorrect :: [Program] -> Int
findCorrect [] = (-1)
findCorrect (p:rest) =
    case runProgram p of
        Left _ -> findCorrect rest
        Right acc -> acc

main :: IO ()
main = do
    program <- readAll "days/Inputs/Day8.txt"
    putStrLn $ show $ fromLeft (-1) $ runProgram program
    putStrLn $ show $ findCorrect $ mutations program
