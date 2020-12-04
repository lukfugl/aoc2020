module IntCode (runComputation, flashMemory, setInput, continue) where

import Text.ParserCombinators.ReadP hiding (get)
import Text.Read.Lex
import Control.Monad.State.Lazy
import Data.Sequence

type ComputationIO = (IO Int, Int -> IO ())
type ComputationState = (Int, Seq Int, ComputationIO)
type Computation = StateT ComputationState IO

-- generic parsing helpers, should probably move to a different library file

intP :: ReadP Int
intP = readDecP +++ (char '-' >> fmap negate readDecP)

commaSeparated :: ReadP a -> ReadP [a]
commaSeparated a = sepBy a (char ',')

mustParse :: ReadP a -> String -> a
mustParse p s
    = case readP_to_S (p <* eof) s of
        [(a, "")] -> a
        _ -> error "no parse"

-- IO

basicIO :: ComputationIO
basicIO =
    let r = fmap read getLine
        w = putStrLn . show
    in (r, w)

-- initialize memory from string

loadMemory :: String -> Seq Int
loadMemory = fromList . mustParse (commaSeparated intP)

flashMemory :: String -> Computation ()
flashMemory program = do
    let memory' = loadMemory program
    (pc, _, io) <- get
    put (pc, memory', io)

-- manipulating computation state

load :: Int -> Computation Int
load p = do
    (_, memory, _) <- get
    return (index memory p)

store :: Int -> Int -> Computation ()
store p v = do
    (pc, memory, io) <- get
    let memory' = adjust (const v) p memory
    put (pc, memory', io)

advance :: Int -> Computation ()
advance n = do
    (pc, memory, io) <- get
    put (pc + n, memory, io)

jump :: Int -> Computation ()
jump pc' = do
    (_, memory, io) <- get
    put (pc', memory, io)

setInput :: IO Int -> Computation ()
setInput r' = do
    (pc, memory, (_, w)) <- get
    put (pc, memory, (r', w))

-- interpreting opcodes

data OpParam
    = Pos Int
    | Imm Int
    deriving Show

data OpCode
    = Add OpParam OpParam OpParam
    | Multiply OpParam OpParam OpParam
    | Input OpParam
    | Output OpParam
    | JumpNZ OpParam OpParam
    | JumpZ OpParam OpParam
    | LessThan OpParam OpParam OpParam
    | Equal OpParam OpParam OpParam
    | Halt
    deriving Show

nextOp' :: Int -> Seq Int -> OpCode
nextOp' pc memory =
    let (flags, opcode) = (index memory pc) `divMod` 100
        param = \n ->
            let m = flags `mod` (10 ^ n) `div` (10 ^ (n - 1))
                p = if m == 0 then Pos else Imm
                a = index memory (pc + n)
            in p a
        pack1 = ($ param 1)
        pack2 = flip pack1 (param 2)
        pack3 = flip pack2 (param 3)
    in case opcode of
        1 -> pack3 Add
        2 -> pack3 Multiply
        3 -> pack1 Input
        4 -> pack1 Output
        5 -> pack2 JumpNZ
        6 -> pack2 JumpZ
        7 -> pack3 LessThan
        8 -> pack3 Equal
        99 -> Halt

nextOp :: Computation OpCode
nextOp = do
    (pc, memory, _) <- get
    return $ nextOp' pc memory

resolve :: OpParam -> Computation Int
resolve (Imm v) = return v
resolve (Pos p) = do
    (_, memory, _) <- get
    return (index memory p)

-- evaluating the program

continue :: Computation ()
continue = do
    op <- nextOp
    case op of
        Halt -> return ()
        _ -> do
            executeOp op
            continue

executeOp :: OpCode -> Computation ()
executeOp (Add a b c) = binaryOp (+) a b c
executeOp (Multiply a b c) = binaryOp (*) a b c
executeOp (JumpNZ a b) = jumpOp (/= 0) a b
executeOp (JumpZ a b) = jumpOp (== 0) a b
executeOp (LessThan a b c) = compareOp LT a b c
executeOp (Equal a b c) = compareOp EQ a b c

executeOp (Input (Pos p)) = do
    (pc, memory, (r, w)) <- get
    v <- liftIO r
    store p v
    advance 2

executeOp (Output a) = do
    (_, _, (_, w)) <- get
    v <- resolve a
    liftIO $ w v
    advance 2

binaryOp :: (Int -> Int -> Int) -> OpParam -> OpParam -> OpParam -> Computation ()
binaryOp f a b (Pos p) = do
    x <- resolve a
    y <- resolve b
    store p $ f x y
    advance 4

jumpOp :: (Int -> Bool) -> OpParam -> OpParam -> Computation ()
jumpOp pred a b = do
    v <- resolve a
    pc' <- resolve b
    if pred v
        then jump pc'
        else advance 3

compareOp :: Ordering -> OpParam -> OpParam -> OpParam -> Computation ()
compareOp desired a b (Pos p) = do
    x <- resolve a
    y <- resolve b
    if compare x y == desired
        then store p 1
        else store p 0
    advance 4

-- running a computation

runComputation :: Computation () -> IO ComputationState
runComputation c = execStateT c (0, empty, basicIO)