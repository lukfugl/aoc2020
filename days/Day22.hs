module Main where

import Inputs.Helpers (loadStringGroups)
import Control.Monad.State.Lazy
import qualified Data.Set as Set
import Control.Monad.IO.Class (liftIO)

type Deck = [Int]
data Game = Game (Deck, Deck) (Set.Set (Deck, Deck))

readInput :: IO Game
readInput = do
    decks <- fmap (map (map read . drop 1)) $ loadStringGroups "days/Inputs/Day22.txt"
    let deck1 = decks !! 0
    let deck2 = decks !! 1
    return $ Game (deck1, deck2) Set.empty

vanillaCombat :: State Game (Maybe Bool)
vanillaCombat = do
    (Game decks seen) <- get
    case decks of
        (deck, []) -> return $ Just True
        ([], deck) -> return $ Just False
        (card1:deck1, card2:deck2) -> do
            put $ if card1 > card2
                then Game (deck1 ++ [card1, card2], deck2) seen
                else Game (deck1, deck2 ++ [card2, card1]) seen
            return Nothing

-- Nothing = continue game
-- Just True = left wins game
-- Just False = right wins game
playRecursiveGame :: State Game Bool
playRecursiveGame = do
    (Game (deck1, deck2) _) <- get
    let deck1' = take (head deck1) $ tail deck1
    let deck2' = take (head deck2) $ tail deck2
    put $ Game (deck1', deck2') Set.empty
    playGame recursiveCombat

recursiveCombat :: State Game (Maybe Bool)
recursiveCombat = do
    (Game decks seen) <- get
    if Set.member decks seen
        then return $ Just True
        else case decks of
            (deck, []) -> return $ Just True
            ([], deck) -> return $ Just False
            (card1:deck1, card2:deck2) -> do
                let seen' = Set.insert decks seen
                roundWinner <- if card1 > length deck1 || card2 > length deck2
                    then return $ card1 > card2
                    else playRecursiveGame
                put $ if roundWinner
                    then Game (deck1 ++ [card1, card2], deck2) seen'
                    else Game (deck1, deck2 ++ [card2, card1]) seen'
                return Nothing

playGame :: State Game (Maybe Bool) -> State Game Bool
playGame runGame = do
    winner <- runGame
    case winner of
        Nothing -> playGame runGame
        Just winner -> return winner

winningDeck :: State Game (Maybe Bool) -> State Game Deck
winningDeck runGame = do
    winner <- playGame runGame
    (Game (deck1, deck2) _) <- get
    return $ if winner then deck1 else deck2

score :: Deck -> Int
score deck = sum $ map (uncurry (*)) $ zip (reverse deck) [1..]

main :: IO ()
main = do
    input <- readInput
    let part1 = score $ evalState (winningDeck vanillaCombat) input
    putStrLn $ show part1
    let part2 = score $ evalState (winningDeck recursiveCombat) input
    putStrLn $ show part2
