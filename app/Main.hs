module Main where

-- This main function will be used to experiment wth profiling
main :: IO ()
main = putStrLn $ show result

result :: Integer
result = foldr (*) 1 [1 .. 100000]
