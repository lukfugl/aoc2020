module Main where

import Inputs.Helpers (loadStringGroups)
import qualified Data.Map as M
import qualified Control.Monad.State.Lazy as S
import Text.ParserCombinators.ReadP
import Text.Read.Lex

data Rule
    = Term Char
    | Choice [Rule]
    | Sequence [Rule]
    | RuleIdx Int
    | Compiled (ReadP ())

type RuleSet = M.Map Int Rule

data RuleInput = RuleInput { asPair :: (Int, Rule) }

ruleIdxP :: ReadP Rule
ruleIdxP = fmap RuleIdx readDecP

ruleSequenceP :: ReadP Rule
ruleSequenceP = fmap Sequence $ sepBy1 ruleIdxP (char ' ')

ruleChoiceP :: ReadP Rule
ruleChoiceP = fmap Choice $ sepBy1 ruleSequenceP (string " | ")

ruleTermP :: ReadP Rule
ruleTermP = fmap Term $ between (char '"') (char '"') get

ruleInputP :: ReadP RuleInput
ruleInputP = do
    idx <- readDecP
    string ": "
    rule <- choice [ruleChoiceP, ruleTermP]
    return $ RuleInput (idx, rule)

instance Read RuleInput where
    readsPrec _ = readP_to_S ruleInputP

getRuleByIndex :: Int -> S.State RuleSet Rule
getRuleByIndex k = do
    rules <- S.get
    return $ (M.!) rules k

setRuleByIndex :: Int -> Rule -> S.State RuleSet Rule
setRuleByIndex k rule = do
    rules <- S.get
    let rules' = M.insert k rule rules
    S.put rules'
    return rule

compileRule :: Rule -> S.State RuleSet (ReadP ())
compileRule (Compiled parser) = return parser
compileRule (RuleIdx k) = compileIndex k
compileRule (Term ch) = return $ char ch >> return ()
compileRule (Sequence rules) = mapM compileRule rules >>= return . foldl1 (>>)
compileRule (Choice rules) = mapM compileRule rules >>= return . foldl1 (+++)

compileIndex :: Int -> S.State RuleSet (ReadP ())
compileIndex k = do
    current <- getRuleByIndex k
    case current of
        Compiled parser -> return parser
        _ -> do
            parser <- compileRule current
            setRuleByIndex k $ Compiled parser
            return parser

part1 :: S.State RuleSet (ReadP ())
part1 = do
    parser <- compileIndex 0
    return $ parser <* eof

part2 :: S.State RuleSet (ReadP ())
part2 = do
    parse42 <- compileIndex 42
    parse31 <- compileIndex 31
    let parse8 = skipMany1 parse42
    let parse11 = do
            ns <- many1 parse42
            count (length ns) parse31
            return ()
    setRuleByIndex 8 (Compiled parse8)
    setRuleByIndex 11 (Compiled parse11)
    part1

readInput :: String -> IO (RuleSet, [String])
readInput path = do
    [rawRules, messages] <- loadStringGroups path
    let rules = M.fromList $ map (asPair . read) rawRules
    return (rules, messages)

checkParse :: ReadP () -> String -> Bool
checkParse p s = readP_to_S p s /= []

main :: IO ()
main = do
    (rules, messages) <- readInput "days/Inputs/Day19.txt"

    let parser = S.evalState part1 rules
    let answer = length $ filter (checkParse parser) messages
    putStrLn $ show answer
    
    let parser' = S.evalState part2 rules
    let answer' = length $ filter (checkParse parser') messages
    putStrLn $ show answer'
