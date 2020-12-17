module Main where

import Inputs.Helpers (loadStrings)

import Data.Ix (range, inRange)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

-- the life data structure
data Life
    = Cell Bool
    | Nested [Life] Int
    deriving Show

type Coordinate = [Int]
type Region = [[Int]]

-- what are the dimensions of the n-dimensional life
size :: Life -> Coordinate
size (Cell _) = []
size (Nested (slice:_) n) = (n:size slice)

-- get a (n-1)-dimensional slice of the n-dimensional life, if in bounds
slice :: Life -> Int -> Maybe Life
slice (Cell _) _ = Nothing
slice (Nested slices n) i
    | inRange (0, n-1) i = Just (slices !! i)
    | otherwise = Nothing

-- see if a cell in the n-dimensional life is alive
alive :: Coordinate -> Life -> Bool
alive _ (Cell alive) = alive
alive (i:js) life = maybe False (alive js) (slice life i)

-- see how many cells in a region of the n-dimensional life are alive
countRegion :: Region -> Life -> Int
countRegion _ (Cell True) = 1
countRegion _ (Cell False) = 0
countRegion (is:js) life =
    let slices = catMaybes $ map (slice life) is
    in sum $ map (countRegion js) slices

-- see how many cells in the whole n-dimensional life are alive
countAll :: Life -> Int
countAll life = countRegion (map (\n -> [0..n-1]) $ size life) life

-- create a region at +/-1 around the coordinate in all dimensions
neighborhood :: Coordinate -> Region
neighborhood [] = []
neighborhood (i:js) = ([(i-1)..(i+1)]:neighborhood js)

-- see if a coordinate should come to alive in the next generation
evaluate :: Life -> Coordinate -> Life
evaluate life coord =
    case (alive coord life, countRegion (neighborhood coord) life) of
        (True, 4) -> Cell True
        (_, 3) -> Cell True
        _ -> Cell False

-- build the next generation of an n-dimensional life
runGeneration :: Life -> Life
runGeneration life =
    let sizes = map (\n -> [-1..n]) $ size life
    in build sizes []
    where build [] coord = evaluate life $ reverse coord
          build (is:js) coord = Nested (map (build js . (:coord)) is) (length is)

-- iterate n generations
runGenerations :: Life -> Int -> Life
runGenerations life 0 = life
runGenerations life n = runGenerations (runGeneration life) (n-1)

-- parsing input
cellP :: ReadP Life
cellP = choice [ char '#' >> return (Cell True)
               , char '.' >> return (Cell False)
               ]

rowP :: ReadP Life
rowP = do
    cells <- manyTill cellP eof
    return $ Nested cells (length cells)

instance Read Life where
    readsPrec _ = readP_to_S rowP

-- actual application
main = do
    input <- loadStrings "days/Inputs/Day17.txt"
    let rows = map read input
    let threeD = Nested [Nested rows (length rows)] 1
    let fourD = Nested [threeD] 1
    putStrLn $ show $ countAll $ runGenerations threeD 6
    putStrLn $ show $ countAll $ runGenerations fourD 6
