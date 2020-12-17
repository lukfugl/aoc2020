module Main where

import Inputs.Helpers (loadStringGroups)
import Text.ParserCombinators.ReadP
import Text.Read.Lex (readDecP)
import Data.Maybe

type Range = (Int, Int)
type Domain = [Range]
data Field
    = Field String Domain
    deriving (Show)
data Ticket
    = Ticket [Int]
    deriving (Show)

rangeP :: ReadP Range
rangeP = do
    i <- readDecP
    char '-'
    j <- readDecP
    return (i, j)

domainP :: ReadP Domain
domainP = do
    r1 <- rangeP
    string " or "
    r2 <- rangeP
    return [r1, r2]

fieldP :: ReadP Field
fieldP = do
    name <- manyTill (satisfy $ const True) (char ':')
    skipSpaces
    domain <- domainP
    return $ Field name domain

instance Read Field where
    readsPrec _ = readP_to_S fieldP

ticketP :: ReadP Ticket
ticketP = do
    ns <- sepBy1 readDecP (char ',')
    return $ Ticket ns

instance Read Ticket where
    readsPrec _ = readP_to_S ticketP

readInput :: IO ([Field], Ticket, [Ticket])
readInput = do
    groups <- loadStringGroups "days/Inputs/Day16.txt"
    let fields = map read (groups !! 0)
    let myTicket = read ((groups !! 1) !! 1)
    let otherTickets = map read (tail (groups !! 2))
    return (fields, myTicket, otherTickets)

mergeDomains :: Domain -> Domain -> Domain
mergeDomains [] others = others
mergeDomains [(a, b)] [] = [(a, b)]
mergeDomains [(a, b)] ((c, d):rest)
    | b < c = ((a, b):(c, d):rest)
    | d < a = ((c, d):mergeDomains [(a, b)] rest)
    | otherwise = mergeDomains [(min a c, max b d)] rest
mergeDomains ((a, b):rest) others = mergeDomains [(a, b)] $ mergeDomains rest others
    
fullDomain :: [Field] -> Domain
fullDomain [] = []
fullDomain (Field _ domain:rest) = mergeDomains domain $ fullDomain rest

inDomain :: Domain -> Int -> Bool
inDomain [] _ = False
inDomain ((a, b):rest) n
    | n < a = False
    | n <= b = True
    | otherwise = inDomain rest n

-- returns any invalid values from tickets
checkValid :: [Field] -> Ticket -> [Int]
checkValid fields (Ticket ns) =
    let domain = fullDomain fields
    in filter (not . inDomain domain) ns

cleanTickets :: [Field] -> [Ticket] -> [Ticket]
cleanTickets fields tickets = catMaybes $ map check tickets
    where check t =
            case checkValid fields t of
                [] -> Just t
                _ -> Nothing

index :: Int -> Ticket -> Int
index i (Ticket ns) = ns !! i

g i tickets (Field name domain) =
    if all (inDomain domain . index i) tickets
        then Just name
        else Nothing

main :: IO ()
main = do
    (fields, myTicket, otherTickets) <- readInput
    putStrLn . show . sum $ concatMap (checkValid fields) otherTickets
    let cleaned = cleanTickets fields otherTickets
    let tickets = (myTicket:cleaned)
    mapM_ (putStrLn . show) $ map (\i -> catMaybes $ map (g i tickets) fields) [0..19] 
    putStrLn . show . product $ map (\i -> index i myTicket) [1, 3, 9, 13, 15, 17]

{-
 0: train
 1: departure track
 2: wagon
 3: departure location
 4: duration
 5: arrival platform
 6: type
 7: route
 8: price
 9: departure platform
10: zone
11: seat
12: arrival track
13: departure date
14: class
15: departure time
16: arrival station
17: departure station
18: arrival location
19: row
-}