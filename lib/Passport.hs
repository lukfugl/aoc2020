module Passport (Passport, readPassport, semiValidPassport, validPassport) where

import Text.ParserCombinators.ReadP
import Text.Read.Lex
import Text.Read hiding ((+++))
import Data.Char

data Passport = Passport
    { birthYear :: Maybe String -- byr
    , issueYear :: Maybe String -- iyr
    , expirationYear :: Maybe String -- eyr
    , height :: Maybe String -- hgt
    , hairColor :: Maybe String -- hcl
    , eyeColor :: Maybe String -- ecl
    , passportID :: Maybe String -- pid
    , countryID :: Maybe String -- cid
    } deriving (Show)

blankPassport :: Passport
blankPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

nonWhitespaceP :: ReadP String
nonWhitespaceP = many $ satisfy (not . isSpace)

consumeField :: String -> (String -> Passport) -> ReadP Passport
consumeField token f
    = string (token ++ ":")
    >> fmap f (nonWhitespaceP <* skipSpaces)
    >>= fillPassportP

fillPassportP :: Passport -> ReadP Passport
fillPassportP passport
    =   consumeField "byr" (\s -> passport { birthYear = Just s })
    +++ consumeField "iyr" (\s -> passport { issueYear = Just s })
    +++ consumeField "eyr" (\s -> passport { expirationYear = Just s })
    +++ consumeField "hgt" (\s -> passport { height = Just s })
    +++ consumeField "hcl" (\s -> passport { hairColor = Just s })
    +++ consumeField "ecl" (\s -> passport { eyeColor = Just s })
    +++ consumeField "pid" (\s -> passport { passportID = Just s })
    +++ consumeField "cid" (\s -> passport { countryID = Just s })
    +++ (eof >> return passport)

passportP :: ReadP Passport
passportP = skipSpaces >> fillPassportP blankPassport

maybeParse :: String -> ReadP a -> Maybe a
maybeParse s p = case readP_to_S (p <* eof) s of
    [(a, "")] -> return a
    _ -> Nothing

readPassport :: String -> Passport
readPassport s = case maybeParse s passportP of
    Just passport -> passport
    _ -> error $ "no parse: " ++ s

semiValidPassport :: Passport -> Bool
semiValidPassport passport
    = case monadic of
        Nothing -> False
        Just _ -> True
    where
        monadic = do
            birthYear passport
            issueYear passport
            expirationYear passport
            height passport
            hairColor passport
            eyeColor passport
            passportID passport

validPassport :: Passport -> Bool
validPassport passport
    = case monadic of
        Nothing -> False
        Just _ -> True
    where
        monadic = do
            birthYear passport >>= numBetween 1920 2002
            issueYear passport >>= numBetween 2010 2020
            expirationYear passport >>= numBetween 2020 2030
            height passport >>= validHeight
            hairColor passport >>= validRGBColor
            eyeColor passport >>= validTextColor
            passportID passport >>= validPassportID
        numBetween a b s = do
            c <- (readMaybe s) :: Maybe Int
            check $ c >= a && c <= b
        validHeight s = do
            (h, metric) <- maybeParse s $ do
                n <- many $ satisfy isDigit
                m <- string "in" +++ string "cm"
                return (n, m)
            case metric of
                "cm" -> numBetween 150 193 h
                "in" -> numBetween 59 76 h
        validRGBColor = checkParse (char '#' >> (count 6 $ satisfy (`elem` "0123456789abcdef")))
        validTextColor = checkElem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validPassportID = checkParse (count 9 $ satisfy isDigit)
        checkParse p s = maybeParse s (p >> return ())
        checkElem lst s = check $ s `elem` lst
        check True = return ()
        check False = Nothing
