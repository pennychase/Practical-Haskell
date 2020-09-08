module Chapter7.Exercises where

import Data.List

brokenThreeJumps y as bs cs =
    let x = brokenThreeJumps' as bs cs
    in map ((y +) . head) $ group $ sort x

brokenThreeJumps' as bs cs = do
    a <- as
    b <- bs
    c <- cs
    return $ a + b + c

brokenJumps :: Int -> Int -> [Int] -> [Int]
brokenJumps year n jumps =
    let x = brokenJumps' n jumps jumps
    in map ((year +) . head) $ group $ sort x

brokenJumps' :: Int -> [Int] -> [Int] -> [Int]
brokenJumps' n jumps combinations = do
    if n == 0
    then return $ combinations 
    else do
        j <- jumps
        c <- combinations
        r <- j + c
        return $ brokenJumps' (n-1) (j+c) 
 


