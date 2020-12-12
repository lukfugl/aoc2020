module Main where

import Inputs.Helpers (readAll)

-- core data types
data IVec2 = IVec2 Int Int

data Direction
    = North
    | South
    | East
    | West

data Command
    = Move Direction Int
    | Rotate Int
    | Forward Int

instance Read Command where
    readsPrec _ [] = []
    readsPrec k s = do
        (n, s') <- readsPrec k $ tail s
        command <- case head s of
            'N' -> return $ Move North n
            'S' -> return $ Move South n
            'E' -> return $ Move East n
            'W' -> return $ Move West n
            'L' -> return $ Rotate n
            'R' -> return $ Rotate (-n)
            'F' -> return $ Forward n
            _ -> []
        return (command, s')

-- "I know how to move a ___ by some (dx, dy)"
class Moveable a where
    move :: IVec2 -> a -> a

instance Moveable IVec2 where
    move (IVec2 dx dy) (IVec2 x y) = IVec2 (x + dx) (y + dy)

-- "I know how to make a (dx, dy) representing n copies of ___, and apply said shift"
class Shiftable a where
    asShift :: a -> Int -> IVec2
    shift :: Moveable b => a -> Int -> b -> b
    shift a n = move (asShift a n)

instance Shiftable IVec2 where
    asShift (IVec2 x y) n = IVec2 (n * x) (n * y)

instance Shiftable Direction where
    asShift North = asShift $ IVec2 0 1
    asShift South = asShift $ IVec2 0 (-1)
    asShift East = asShift $ IVec2 1 0
    asShift West = asShift $ IVec2 (-1) 0

-- "I know how to rotate a ___ by 90 degrees (and multiples)"
class Rotateable a where
    rotate90 :: a -> a
    rotate :: Int -> a -> a
    rotate 90 = rotate90
    rotate 180 = rotate90 . rotate 90
    rotate 270 = rotate90 . rotate 180
    rotate (-90) = rotate 270
    rotate (-180) = rotate 180
    rotate (-270) = rotate 90

instance Rotateable IVec2 where
    rotate90 (IVec2 x y) = IVec2 (-y) x

instance Rotateable Direction where
    rotate90 East = North
    rotate90 North = West
    rotate90 West = South
    rotate90 South = East

-- "I know how to move a ___ forward n times"
class Forwardable a where
    forward :: Int -> a -> a

-- "I know how to apply commands to a ___"
class Commandable a where
    followCommand :: Command -> a -> a
    followCommands :: [Command] -> a -> a
    followCommands = (flip . foldl . flip) followCommand

-- "I know how to measure a ___"
class Measurable a where
    measure :: a -> Int

instance Measurable IVec2 where
    measure (IVec2 x y) = abs x + abs y

-- and now the interpreter of commands and its instances of the applicable classes
data Navigator
    = Bad IVec2 Direction
    | Good IVec2 IVec2

instance Moveable Navigator where
    move d (Bad location heading) = Bad (move d location) heading
    move d (Good location waypoint) = Good location (move d waypoint)

instance Rotateable Navigator where
    rotate90 (Bad location heading) = Bad location (rotate90 heading)
    rotate90 (Good location waypoint) = Good location (rotate90 waypoint)

instance Forwardable Navigator where
    forward n (Bad location heading) = Bad (shift heading n location) heading
    forward n (Good location waypoint) = Good (shift waypoint n location) waypoint

instance Commandable Navigator where
    followCommand (Move direction n) = shift direction n
    followCommand (Rotate n) = rotate n
    followCommand (Forward n) = forward n

instance Measurable Navigator where
    measure (Bad location _) = measure location
    measure (Good location _) = measure location

-- use the system we've built!
evaluate :: (Measurable a, Commandable a) => [Command] -> a -> IO ()
evaluate commands = putStrLn . show . measure . followCommands commands

navigators :: [Navigator]
navigators =
    [ Bad (IVec2 0 0) East -- part 1
    , Good (IVec2 0 0) (IVec2 10 1) -- part 2
    ]

main :: IO ()
main = do
    commands <- readAll "days/inputs/Day12.txt"
    mapM_ (evaluate commands) navigators
