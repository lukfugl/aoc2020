module Main where

import Inputs.Helpers (readAll)
import Text.ParserCombinators.ReadP
import Data.Char (isSpace)
import Data.List (union, intersect, (\\), sort)

data Foodstuff = Foodstuff
    { ingredients :: [String]
    , allergens :: [String]
    } deriving Show

instance Read Foodstuff where
    readsPrec _ = readP_to_S $ do
        let word = many1 $ satisfy $ not . isSpace
        ingredients <- sepBy1 word (char ' ')
        allergens <- between (string " (contains ") (char ')') (sepBy1 word (string ", "))
        return $ Foodstuff ingredients allergens

readInput :: IO [Foodstuff]
readInput = readAll "days/Inputs/Day21.txt"

allAllergens :: [Foodstuff] -> [String]
allAllergens = foldl1 union . map allergens

possibleIngredients :: String -> [Foodstuff] -> [String]
possibleIngredients allergen [] = []
possibleIngredients allergen (food:foods) =
    let restriction = possibleIngredients allergen foods 
    in if allergen `elem` allergens food
        then case restriction of
            [] -> ingredients food
            _ -> intersect restriction $ ingredients food
        else restriction

countOtherIngredients :: [String] -> Foodstuff -> Int
countOtherIngredients exclude food = length (ingredients food \\ exclude)

solve :: [(String, [String])] -> [(String, String)]
solve [] = []
solve constraints =
    let fixed = filter (\(allergen, ingredients) -> length ingredients == 1) constraints
        fixed' = map (\(allergen, ingredients) -> (allergen, head ingredients)) fixed
        fixedIngredients = concatMap snd fixed
        constraints' = map (\(allergen, ingredients) -> (allergen, ingredients \\ fixedIngredients)) $ (constraints \\ fixed)
    in fixed' ++ solve constraints'

commaSep :: [String] -> String
commaSep [a] = a
commaSep (a:as) = a ++ "," ++ commaSep as

main :: IO ()
main = do
    input <- readInput
    let allergens = allAllergens input
    let constraints = map (\allergen -> (allergen, possibleIngredients allergen input)) allergens
    let solved = solve constraints
    let badIngredients = map snd $ sort solved
    let part1 = sum $ map (countOtherIngredients badIngredients) input
    putStrLn $ show part1
    putStrLn $ commaSep badIngredients