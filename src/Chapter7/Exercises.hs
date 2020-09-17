module Chapter7.Exercises where

import Data.List
import Control.Monad

-- My original version
brokenThreeJumps :: Int -> [Int] -> [Int] -> [Int] -> [Int]
brokenThreeJumps y as bs cs =
    let x = brokenThreeJumps' as bs cs
    in map ((y +) . head) $ group $ sort x
    where
        brokenThreeJumps' :: [Int] -> [Int] -> [Int] -> [Int]
        brokenThreeJumps' as bs cs = do
            a <- as
            b <- bs
            c <- cs
            return $ a + b + c

-- After learning about nub
brokenThreeJumps2 :: Int -> [Int] -> [Int]
brokenThreeJumps2 year jumps = 
    nub $ map (+ year) js
    where
        js = do
            a <- jumps
            b <- jumps
            c <- jumps
            return $ a + b + c

brokenJumps :: Int -> Int -> [Int] -> [Int]
brokenJumps year n jumps =
    let x = brokenJumps' (replicate (n) jumps) (replicate (length jumps) 0)
    in map ((year +) . head) $ group $ sort x
    where
        brokenJumps' :: [[Int]] -> [Int] -> [Int]
        brokenJumps' [] acc = acc
        brokenJumps' (xs:xss) acc =
            let newAcc = brokenJumps' xss acc
            in [ x + y | x <- xs, y <- newAcc ]

brokenJumps2 :: Int -> Int -> [Int] -> [Int]
brokenJumps2 year n jumps =
    nub $ map (\js -> year + sum js) $ replicateM n jumps
