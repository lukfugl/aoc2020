module GameCode where

import Text.ParserCombinators.ReadP
import Text.Read.Lex

data Op
    = Acc
    | Jmp
    | Nop
    deriving (Show)

lexOp :: ReadP Op
lexOp = choice
    [ string "acc" >> return Acc
    , string "jmp" >> return Jmp
    , string "nop" >> return Nop
    ]

instance Read Op where
    readsPrec _ = readP_to_S lexOp

data Instruction
    = Instruction Op Int
    deriving (Show)

lexInstruction :: ReadP Instruction
lexInstruction = do
    op <- lexOp
    skipSpaces
    n <- choice
        [ char '+' >> readDecP
        , char '-' >> fmap negate readDecP
        ]
    return $ Instruction op n

instance Read Instruction where
    readsPrec _ = readP_to_S lexInstruction

type Program = [Instruction]

runProgram :: Program -> Either Int Int
runProgram program = runProgram' [] (0, 0)
    where runProgram' seen (pc, acc)
            | pc `elem` seen = Left acc
            | pc >= length program = Right acc
            | otherwise = runProgram' (pc:seen) $ execute (instruction pc) (pc, acc)
          instruction idx = program !! idx

execute :: Instruction -> (Int, Int) -> (Int, Int)
execute (Instruction Acc n) (pc, acc) = (pc + 1, acc + n)
execute (Instruction Jmp k) (pc, acc) = (pc + k, acc)
execute (Instruction Nop _) (pc, acc) = (pc + 1, acc)
