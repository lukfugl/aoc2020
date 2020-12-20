module Main where

import Inputs.Helpers (loadStringGroups)
import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Data.Maybe
import Debug.Trace
import Data.List

type Image = [String]
data Tile = Tile Int Image

tileID :: Tile -> Int
tileID (Tile id _) = id

headerP :: ReadP Int
headerP = do
    string "Tile "
    id <- readDecP
    char ':'
    return id

readHeader :: String -> Int
readHeader s = case readP_to_S headerP s of
    [(id, "")] -> id
    _ -> error "no parse"

rotateClockwise :: Image -> Image
rotateClockwise lines =
    if (length $ head lines) > 0
        then let top = map head lines
                 rest = rotateClockwise $ map tail lines
             in (top:rest)
        else []

flipVertical :: Image -> Image
flipVertical = reverse

flipHorizontal :: Image -> Image
flipHorizontal = map reverse

symmetries :: Image -> [Image]
symmetries image =
    let image2 = rotateClockwise image
        image3 = flipVertical image
        image4 = flipHorizontal image2
        image5 = flipVertical image4
        image6 = flipHorizontal image
        image7 = flipHorizontal image5
        image8 = flipHorizontal image3
    in [image, image2, image3, image4, image5, image6, image7, image8]

asTiles :: [String] -> [Tile]
asTiles (header:image) = map (Tile $ readHeader header) $ symmetries image

select :: [Tile] -> Tile -> (Tile, [Tile])
select tiles tile =
    let keep = filter (\tile2 -> tileID tile2 /= tileID tile) tiles
    in (tile, keep)

matchHorizontalImage :: Image -> Image -> Bool
matchHorizontalImage image1 image2 = map last image1 == map head image2

matchVerticalImage :: Image -> Image -> Bool
matchVerticalImage image1 image2 = last image1 == head image2

matchHorizontal :: Tile -> Tile -> Bool
matchHorizontal (Tile _ image1) (Tile _ image2) = matchHorizontalImage image1 image2

matchVertical :: Tile -> Tile -> Bool
matchVertical (Tile _ image1) (Tile _ image2) = matchVerticalImage image1 image2

alignsLeft :: Int -> [Tile] -> Tile -> Bool
alignsLeft d arranged tile
    | (length arranged `mod` d) == 0 = True
    | otherwise =
        let left = last arranged
        in matchHorizontal left tile

alignsAbove :: Int -> [Tile] -> Tile -> Bool
alignsAbove d arranged tile
    | length arranged < d = True
    | otherwise =
        let above = arranged !! (length arranged - d)
        in matchVertical above tile

aligns :: Int -> [Tile] -> Tile -> Bool
aligns d arranged tile = alignsLeft d arranged tile && alignsAbove d arranged tile

trySelection :: Int -> [Tile] -> (Tile, [Tile]) -> Maybe [Tile]
trySelection d arranged (candidate, rest) =
    if aligns d arranged candidate
        then arrange d rest (arranged ++ [candidate])
        else Nothing

arrange :: Int -> [Tile] -> [Tile] -> Maybe [Tile]
arrange _ [] arranged = Just arranged
arrange d remaining arranged =
    let selections = map (select remaining) remaining
        results = map (trySelection d arranged) selections
    in case catMaybes results of
        (res:_) -> Just res
        [] -> Nothing

readInput :: IO [Tile]
readInput = do
    groups <- loadStringGroups "days/Inputs/Day20.txt"
    return $ concatMap asTiles groups

getLayout :: [Tile] -> [Tile]
getLayout tiles =
    case arrange 12 tiles [] of
        Nothing -> error "no layout found"
        Just layout -> layout

getCorners :: [Tile] -> [Tile]
getCorners layout =
    let firstRow = take 12 layout
        lastRow = drop 132 layout
        topLeft = head firstRow
        topRight = last firstRow
        bottomLeft = head lastRow
        bottomRight = last lastRow
    in [topLeft, topRight, bottomLeft, bottomRight]

innerImage :: Tile -> Image
innerImage (Tile _ image) = map (tail . init) $ tail $ init image

joinHorizontal :: [Image] -> Image
joinHorizontal images =
    if length (head images) == 0
        then []
        else let row = foldl1 (++) $ map head images
                 rest = joinHorizontal $ map tail images
             in (row:rest)

formImage :: [Tile] -> Image
formImage [] = []
formImage layout =
    let row = joinHorizontal $ map innerImage $ take 12 layout
    in row ++ (formImage $ drop 12 layout)

seaMonster :: Image
seaMonster =
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   "
    ]

isSeaMonster :: Image -> (Int, Int) -> Bool
isSeaMonster image (x0, y0) =
    let xs = [0..19]
        ys = [0..2]
        range = concatMap (\dx -> map (\dy -> (dx,dy)) ys) xs
    in all matchPixel range
    where matchPixel (dx, dy) =
            if ((seaMonster !! dy) !! dx) == ' '
                then True
                else ((image !! (y0 + dy)) !! (x0 + dx)) == '#'

countSeaMonsters :: Image -> Int
countSeaMonsters image =
    let xs = [0..76]
        ys = [0..93]
        range = concatMap (\x -> map (\y -> (x,y)) ys) xs
    in length $ filter (isSeaMonster image) range

countHashes :: Image -> Int
countHashes [] = 0
countHashes (row:rest) = countHashes rest + (length $ filter (== '#') row)

main :: IO ()
main = do
    tiles <- readInput
    let layout = getLayout tiles
    let part1 = product $ map tileID $ getCorners layout
    putStrLn $ show part1

    let image = formImage layout
    let seaMonsters = maximum $ map countSeaMonsters $ symmetries image
    let part2 = (countHashes image) - (seaMonsters * (countHashes seaMonster))
    putStrLn $ show part2

