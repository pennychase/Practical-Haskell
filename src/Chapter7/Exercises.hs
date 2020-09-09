module Chapter7.Exercises where

import Data.List

brokenThreeJumps :: Int -> [Int] -> [Int] -> [Int] -> [Int]
brokenThreeJumps y as bs cs =
    let x = brokenThreeJumps' as bs cs
    in map ((y +) . head) $ group $ sort x

brokenThreeJumps' :: [Int] -> [Int] -> [Int] -> [Int]
brokenThreeJumps' as bs cs = do
    a <- as
    b <- bs
    c <- cs
    return $ a + b + c

brokenJumps :: Int -> Int -> [Int] -> [Int]
brokenJumps year n jumps =
    let x = brokenJumps' (replicate (n) jumps) (replicate (length jumps) 0)
    in map ((year +) . head) $ group $ sort x

brokenJumps' :: [[Int]] -> [Int] -> [Int]
brokenJumps' [] acc = acc
brokenJumps' (xs:xss) acc =
    let newAcc = brokenJumps' xss acc
    in [ x + y | x <- xs, y <- newAcc ]

brokenJumps'' :: [[Int]] -> [Int] -> [Int]
brokenJumps'' [] acc = return acc
brokenJumps'' (xs:xss) acc = do
    newAcc <- brokenJumps'' xss acc
    y <- newAcc
    x <- xs
    return $! x + y
