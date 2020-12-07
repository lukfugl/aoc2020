module Inputs.Helpers (loadString, loadStrings, loadStringGroups) where

import System.IO
import Data.Bool

-- reimplemented because I can't build Control.Conditional in WSL
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mcond mtrue mfalse = mcond >>= bool mfalse mtrue

untilM :: Monad m => m Bool -> m a -> m [a]
untilM mcond ma = ifM mcond (return []) $ do
    a <- ma
    as <- untilM mcond ma
    return (a:as)

orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (mcond:rest) = ifM mcond (return True) (orM rest)

-- collect the IO action until EOF
gather :: (Handle -> IO a) -> Handle -> IO [a]
gather hGetA h = untilM (hIsEOF h) (hGetA h)

-- peek if the next line is empty (consuming it if it is)
hBlankLine :: Handle -> IO Bool
hBlankLine h = ifM (fmap (== '\n') $ hLookAhead h) (hGetLine h >> return True) (return False)

-- collect lines until EOF or blank line
hGetStringGroup :: Handle -> IO [String]
hGetStringGroup h = untilM (orM [hIsEOF h, hBlankLine h]) (hGetLine h)

-- return the first line (assumed only) of the file
loadString :: String -> IO String
loadString path = openFile path ReadMode >>= hGetLine

-- return all the lines of the file
loadStrings :: String -> IO [String]
loadStrings path = openFile path ReadMode >>= gather hGetLine

-- break the lines of the file into groups by blank lines
loadStringGroups :: String -> IO [[String]]
loadStringGroups path = openFile path ReadMode >>= gather hGetStringGroup