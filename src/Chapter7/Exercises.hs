module Chapter7.Exercises where

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

-- Exercise 7.1

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

-- Orignal version
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

-- After finding replicateM
brokenJumps2 :: Int -> Int -> [Int] -> [Int]
brokenJumps2 year n jumps =
    nub $ map (\js -> year + sum js) $ replicateM n jumps

-- Exercise 7.2
find_  :: (a -> Bool) -> [a] -> Maybe a
find_ f xs = msum $ map (\x -> case f x of True -> Just x; False -> Nothing) xs
                    
-- Monads

addPrefix :: String -> Reader String String
addPrefix s = ask >>= \p -> return $ p ++ s

-- runReader (addPrefix "hello") "++"  => "++hello"
-- runReader (mapM addPrefix ["hello", "world"]) "++" => ["++hello", "++world"]

logInformation :: [String] -> Writer String ()
logInformation = mapM_ (\s -> tell (s ++ "\n"))

-- runWriter $ logInformation ["hello", "world"] => ((), "hello\nworld\n")

mySequence :: (Monad m) => [ m a] -> m [a]
mySequence [] = return []
mySequence (x:xs) = do
    y <- x
    ys <- mySequence xs
    return (y:ys)

mySequence' :: (Monad m) => [m a] -> m [a]
mySequence' [] = return []
mySequence' (x:xs) = x                  >>= \y ->
                     mySequence' xs     >>= \ys ->
                     return (y:ys)

myMapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
myMapM f [] = return []
myMapM f (x:xs) = do
    y <- f x
    ys <- myMapM f xs
    return (y:ys)

myMapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
myMapM' f = sequence . map f

myFilterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
myFilterM f [] = return []
myFilterM f (x:xs) = do
    y <- f x
    ys <- myFilterM f xs
    if y == True
        then return (x:ys)
        else return ys

factorialSteps :: Integer -> Writer (Sum Integer) Integer
factorialSteps n = foldM (\f x -> tell (Sum 1) >> return (f*x)) 1 [1 .. n]

-- The inner State is the accumulator and the outer StateT is the counter
factorialState' :: StateT Integer (State Integer) Integer
factorialState' = do
    step <- get
    res <- lift $ get
    if step == 0 then return res else do
        lift $ put (res * step)
        put (step - 1)
        factorialState'

factorialState :: Integer -> Integer
factorialState n = execState (execStateT factorialState' n) 1