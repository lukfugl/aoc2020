module Main where

import Data.List

modAdd a b p = (a + b) `mod` p
modMult a b p = (a * b) `mod` p

modPower a 0 p = 1
modPower a 1 p = a
modPower a n p =
    let a' = modPower (modMult a a p) (n `div` 2) p
    in if n `mod` 2 == 0
        then a'
        else modMult a a' p

modInverse p1 p2 = modPower p1 (p2 - 2) p2

mergeConstraint (k1, p1) (k2, p2) = 
    let p' = p1 * p2
        n = modMult (modInverse p1 p2) (k2 - k1) p2
        k' = modAdd (modMult p1 n p') k1 p'
    in (k', p')

main :: IO ()
main = do
    --let buses = [23, 41, 509, 13, 17, 29, 401, 37, 19]
    --flip mapM_ buses $ \bus ->
    --    putStrLn $ show bus ++ ": " ++ (show $ head $ dropWhile (<1004098) $ map (*bus) [1..])
    --putStrLn $ show $ (1004104 - 1004098) * 401
    let kps = [ (0, 23)
              , (13, 41)
              , (23, 509)
              , (36, 13)
              , (37, 17)
              , (52, 29)
              , (54, 401)
              , (60, 37)
              , (73, 19)
              ]
    let (k, p) = foldl1 mergeConstraint kps
    putStrLn $ show $ p - k
