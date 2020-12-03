{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Wiring (Wire(..), readWire, distanceToClosestIntersection) where

import Control.Exception
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP
import Text.Read.Lex

data Direction
    = DirRight
    | DirLeft
    | DirUp
    | DirDown
    deriving (Show, Eq)

data WireExtension
    = ExtendWire Direction Int
    deriving (Show)
    
type Wire = [WireExtension]

lexDirection :: ReadP Direction
lexDirection = do
    d <- lexChar
    case d of
        'R' -> return DirRight
        'L' -> return DirLeft
        'U' -> return DirUp
        'D' -> return DirDown
        _ -> fail "invalid direction"

instance Read Direction where
    readsPrec _ = readP_to_S lexDirection

lexWireExtension :: ReadP WireExtension
lexWireExtension = do
    d <- lexDirection
    n <- readDecP
    return $ ExtendWire d n

instance Read WireExtension where
    readsPrec _ = readP_to_S lexWireExtension

lexWire :: ReadP Wire
lexWire = sepBy lexWireExtension (char ',')

readWire :: String -> Wire
readWire s = checkParse $ readP_to_S lexWire s
    where checkParse [] = error "no parse"
          checkParse ((wire, ""):_) = wire
          checkParse (_:rest) = checkParse rest

distanceToClosestIntersection :: Wire -> Wire -> Int
distanceToClosestIntersection wire1 wire2 =
    let left = asSegments 0 0 wire1
        right = asSegments 0 0 wire2
        intersections = map (\a -> map (intersectSegments a) left) right 
    in head $ sort $ catMaybes $ concat intersections

data WireSegment
    = HorizontalWireSegment Int Int Int -- y x1 x2
    | VerticalWireSegment Int Int Int -- x y1 y2
    deriving (Show)

asSegments :: Int -> Int -> Wire -> [WireSegment]
asSegments _ _ [] = []
asSegments x y ((ExtendWire d n):restOfWire)
    | d == DirRight = (HorizontalWireSegment y (x + 1) (x + n) : asSegments (x + n) y restOfWire)
    | d == DirLeft = (HorizontalWireSegment y (x - n) (x - 1) : asSegments (x - n) y restOfWire)
    | d == DirUp = (VerticalWireSegment x (y + 1) (y + n) : asSegments x (y + n) restOfWire)
    | d == DirDown = (VerticalWireSegment x (y - n) (y - 1) : asSegments x (y - n) restOfWire)

intersectSegments :: WireSegment -> WireSegment -> Maybe Int
intersectSegments (HorizontalWireSegment y1 l1 r1) (HorizontalWireSegment y2 l2 r2)
    | y1 /= y2 = Nothing -- horizontal segments in different rows
    | r1 < l2 || r2 < l1 = Nothing -- non-overlapping horizontal segments in same row
    | otherwise = -- overlapping horizontal segments in same row
        let l = max l1 l2 -- left end of overlap
            r = min r1 r2 -- right end of overlap
            y = abs y1 -- distance to row
        in if l <= 0 && r >= 0
            then return y -- closest point among overlap is at x=0
            else return $ y + min (abs l) (abs r) -- use end of overlap closer to x=0

intersectSegments (VerticalWireSegment x1 b1 t1) (VerticalWireSegment x2 b2 t2)
    | x1 /= x2 = Nothing -- vertical segments in different columns
    | t1 < b2 || t2 < b1 = Nothing -- non-overlapping vertical segments in same column
    | otherwise = -- overlapping vertical segments in same column
        let b = max b1 b2 -- bottom end of overlap
            t = min t1 t2 -- top end of overlap
            x = abs x1 -- distance to column
        in if b <= 0 && t >= 0
            then return x -- closest point among overlap is at y=0
            else return $ x + min (abs b) (abs t) -- use end of overlap closer to y=0

intersectSegments (HorizontalWireSegment y l r) (VerticalWireSegment x b t)
    | x < l || x > r || y < b || y > t = Nothing -- non-intersecting perpendicular segments
    | otherwise = return $ (abs x) + (abs y) -- intersecting perpendicular segments

intersectSegments vertical horizontal
    = intersectSegments horizontal vertical -- use previous definition by flipping parameters
