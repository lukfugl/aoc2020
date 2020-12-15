module Main where

import Text.ParserCombinators.ReadP
import Inputs.Helpers (readAll)
import Text.Read.Lex (readDecP)

data Instruction
    = Mask String
    | Set Int Int
    deriving Show

maskInstructionP :: ReadP Instruction
maskInstructionP = do
    string "mask = "
    s <- manyTill get eof
    return $ Mask s

setInstructionP :: ReadP Instruction
setInstructionP = do
    string "mem["
    i <- readDecP
    string "] = "
    v <- readDecP
    return $ Set i v

instructionP :: ReadP Instruction
instructionP = choice [maskInstructionP, setInstructionP]

instance Read Instruction where
    readsPrec _ = readP_to_S instructionP

-- sure would be nice to have an actual map
data EvalState
    = EvalState String [(Int, Int)]
    deriving Show

setMask :: String -> EvalState -> EvalState
setMask mask (EvalState _ mem) = EvalState mask mem

withBit :: Int -> Int -> Int
withBit n v =
    if v `mod` (2 * n) < n
        then v + n
        else v

withoutBit :: Int -> Int -> Int
withoutBit n v =
    if v `mod` (2 * n) >= n
        then v - n
        else v

applyMask1 :: String -> Int -> Int
applyMask1 mask v = f (2^35) mask v
    where f _ [] v = v
          f n ('X':rest) v = f (n `div` 2) rest v
          f n ('1':rest) v = f (n `div` 2) rest (withBit n v)
          f n ('0':rest) v = f (n `div` 2) rest (withoutBit n v)

setMem1 :: Int -> Int -> EvalState -> EvalState
setMem1 i v (EvalState mask mem) =
    let v' = applyMask1 mask v
        mem' = ((i, v') : filter (\(j, _) -> j /= i) mem)
    in EvalState mask mem'

evalInstruction1 :: Instruction -> EvalState -> EvalState
evalInstruction1 (Mask mask) = setMask mask
evalInstruction1 (Set i v) = setMem1 i v

initState :: EvalState
initState = EvalState "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" []

evalProgram1 :: [Instruction] -> Int
evalProgram1 instructions =
    let (EvalState _ mem) = foldl (flip evalInstruction1) initState instructions
    in sum $ map snd mem

applyMask2 :: String -> Int -> [Int]
applyMask2 mask v = f (2^35) mask v
    where f _ [] v = [v]
          f n ('X':rest) v = (f (n `div` 2) rest (withBit n v)) ++ (f (n `div` 2) rest (withoutBit n v))
          f n ('1':rest) v = f (n `div` 2) rest (withBit n v)
          f n ('0':rest) v = f (n `div` 2) rest v

setMem2 :: Int -> Int -> EvalState -> EvalState
setMem2 i v (EvalState mask mem) =
    let is = applyMask2 mask i
        mem' = (map (\i -> (i, v)) is) ++ (filter (\(j, _) -> j `notElem` is) mem)
    in EvalState mask mem'

evalInstruction2 :: Instruction -> EvalState -> EvalState
evalInstruction2 (Mask mask) = setMask mask
evalInstruction2 (Set i v) = setMem2 i v

evalProgram2 :: [Instruction] -> Int
evalProgram2 instructions =
    let (EvalState _ mem) = foldl (flip evalInstruction2) initState instructions
    in sum $ map snd mem

readInput :: IO [Instruction]
readInput = readAll "days/Inputs/Day14.txt"

main :: IO ()
main = do
    input <- readInput

    putStrLn $ show $ setMem2 26 2 $ setMask "00000000000000000000000000000000X0XX" $ setMem2 42 1 $ setMask "000000000000000000000000000000X1001X" $ initState
    putStrLn $ show $ evalProgram1 input
    putStrLn $ show $ evalProgram2 input