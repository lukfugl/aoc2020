module Luggage (LuggageRule, asGraph, transitiveClosure) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Data.List
import Data.Maybe

data LuggageRule = LuggageRule String [(Int, String)]
    deriving Show

instance Read LuggageRule where
    readsPrec _ = readP_to_S $ do
        container <- many get
        string " bags contain "
        contained <- choice
            [ flip sepBy (string ", ") $ do
                n <- readDecP <* skipSpaces
                adj2 <- many $ satisfy (/= ',')
                choice [string " bags", string " bag"]
                return (n, adj2)
            , string "no other bags" >> return []
            ]
        string "."
        eof
        return $ LuggageRule container contained

type Graph = ([String], Int, [Int])

asGraph :: [LuggageRule] -> Graph
asGraph rules =
    let edges = concatMap asEdges rules
        vertices = asVertices edges
        n = length vertices
        edges' = map (asEdge' n vertices) edges
    in  (vertices, n, edges')
    where
        asEdges (LuggageRule i contained) = map (\(_, j) -> (i, j)) contained
        asVertices [] = []
        asVertices ((i, j):rest) =
            let vertices = asVertices rest
            in [i, j] `union` asVertices rest
        asEdge' n vertices (i, j) =
            let i' = fromJust $ i `elemIndex` vertices
                j' = fromJust $ j `elemIndex` vertices
            in i' * n + j'

transitiveClosure :: Graph -> Graph
transitiveClosure (vertices, n, edges) = (vertices, n, foldl traverseIntermediateVertex edges [0..n-1])
    where
        potentialEdges = [0..n*n - 1]
        traverseIntermediateVertex edges k = foldl (addClosingEdge k) edges potentialEdges
        addClosingEdge k edges ij =
            let (i, j) = ij `divMod` n
                ik = i * n + k
                kj = k * n + j
            in if (ik `elem` edges) && (kj `elem` edges) && (not $ ij `elem` edges)
                then (ij:edges)
                else edges
