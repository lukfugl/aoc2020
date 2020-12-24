module Main where

import Inputs.Helpers (readAll)
import Text.ParserCombinators.ReadP (string, choice, ReadP, readP_to_S, many1)
import qualified Data.Set as Set

-- types
data Dir = E | SE | SW | W | NW | NE
data Path = Path [Dir]
type Tile = (Int, Int)
type Layout = Set.Set Tile

-- reading input
dirP :: ReadP Dir
dirP = choice [ string "e" >> return E
              , string "ne" >> return NE
              , string "nw" >> return NW
              , string "w" >> return W
              , string "sw" >> return SW
              , string "se" >> return SE
              ]

pathP :: ReadP Path
pathP = fmap Path $ many1 dirP

instance Read Path where
    readsPrec _ = readP_to_S pathP

move :: Dir -> Tile -> Tile
move E (x, y) = (x + 2, y)
move NE (x, y) = (x + 1, y + 1)
move NW (x, y) = (x - 1, y + 1)
move W (x, y) = (x - 2, y)
move SW (x, y) = (x - 1, y - 1)
move SE (x, y) = (x + 1, y - 1)

readInput :: IO [Path]
readInput = readAll "days/Inputs/Day24.txt"

-- determining starting layout (part 1)
toTile :: Path -> Tile
toTile (Path dirs) = foldr1 (.) (map move dirs) (0, 0)

flipTile :: Tile -> Layout -> Layout
flipTile tile layout =
    if Set.member tile layout
        then Set.delete tile layout
        else Set.insert tile layout

flipTiles :: [Tile] -> Layout -> Layout
flipTiles tiles = foldr1 (.) $ map flipTile tiles

startingLayout :: [Path] -> Layout
startingLayout input = flipTiles (map toTile input) Set.empty

-- running "life" on layouts
neighbors :: Tile -> [Tile]
neighbors tile = map (flip move tile) [E, NE, NW, W, SW, SE]

countNeighbors :: Layout -> Tile -> Int
countNeighbors layout = length . filter (flip Set.member layout) . neighbors

potentialLayout :: Layout -> Layout
potentialLayout layout = foldl (\layout' -> Set.union layout' . Set.fromList . neighbors) layout layout

filterTile :: Layout -> Tile -> (Layout -> Layout)
filterTile layout tile =
    let n = countNeighbors layout tile
    in if (n == 1 && Set.member tile layout) || (n == 2)
        then Set.insert tile
        else id

nextGeneration :: Layout -> Layout
nextGeneration layout = foldr (filterTile layout) Set.empty (potentialLayout layout)

runGenerations :: Int -> Layout -> Layout
runGenerations n = head . drop n . iterate nextGeneration

-- driver
main :: IO ()
main = do
    input <- readInput
    let initial = startingLayout input
    putStrLn $ show $ Set.size initial
    putStrLn $ show $ Set.size $ runGenerations 100 initial
