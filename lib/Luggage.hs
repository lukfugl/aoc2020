module Luggage (DAG, merge, reverse, reachable, contained) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Data.List hiding (reverse)
import Prelude hiding (reverse)
import Data.Maybe

-- assumed acyclic based on project description
type Edge = (String, String, Int)
data DAG = Graph [Edge]

instance Read DAG where
    readsPrec _ = readP_to_S $ do
        i <- many get
        string " bags contain "
        edges <- choice
            [ flip sepBy (string ", ") $ do
                n <- readDecP <* skipSpaces
                j <- many $ satisfy (/= ',')
                choice [string " bags", string " bag"]
                return (i, j, n)
            , string "no other bags" >> return []
            ]
        string "."
        eof
        return $ Graph edges

merge :: [DAG] -> DAG
merge [] = Graph []
merge (g:rest) = merge' g $ merge rest
    where merge' (Graph a) (Graph b) = Graph $ a ++ b

reverse :: DAG -> DAG
reverse (Graph edges) = Graph $ map reverse' edges
    where reverse' (i, j, n) = (j, i, n)

reachable :: String -> DAG -> [String]
reachable k (Graph edges) = reachable' [] [k]
    where reachable' acc [] = acc
          reachable' acc (k:rest) =
              let children = map target $ filter (shouldVisit acc k) edges
              in reachable' (k:acc) (union rest children)
          target (_, j, _) = j
          hasSource k (i, _, _) = i == k
          unvisited acc (_, j, _) = not (j `elem` acc)
          shouldVisit acc k e = hasSource k e && unvisited acc e

contained :: String -> DAG -> Int
contained k (Graph edges) = contained' k
    where contained' k =
              let edges' = filter (hasSource k) edges              
              in sum $ map (\(_, j, n) -> n * (1 + (contained' j))) edges'
          hasSource k (i, _, _) = i == k
