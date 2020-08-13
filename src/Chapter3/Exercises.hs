{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Chapter3.Exercises where

import Chapter2.TypeExamples
import Chapter3.Ranges

-- Exercise 3.2

filterOnes :: [Integer] -> [Integer]
filterOnes = filter (== 1)

filterANumber :: (Num a, Eq a) =>  a -> [a] -> [a]
filterANumber n = filter (== n)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter $ not . p

filterGovOrgRs :: [ClientR] -> [ClientR]
filterGovOrgRs = filter isGovOrgR
    where
        isGovOrgR (GovOrgR { .. }) = True
        isGovOrgR _                = False

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter (\client -> case client of
                                        (GovOrg _) -> True
                                        _       -> False
                        )

filterGovOrgs' :: [ClientR] -> [ClientR]
filterGovOrgs' = filter (\case (GovOrgR { .. }) -> True ;
                                _               -> False
                        )

-- Ranges
-- Demonstrating smart constructors and view patterns
prettyRange :: Range -> String
prettyRange rng = 
    case rng of
        (r -> R a b) -> "[" ++ show a ++ ".." ++ show b ++ "]"

-- Diving into Lists
-- Write the definition of the list functions

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) = if p x then (x:(myFilter p xs)) else (myFilter p xs)

-- Definig maxiumum 
myMaximum :: (Ord a, Foldable t) => t a -> a
myMaximum = foldl1 max

-- Create  a numeric type with an "identity" for maximum (negative infinity)
data InfNum a =
      MinusInfinity 
    | Number a
    | PlusInfinity
    deriving Show

infMax :: Ord a => InfNum a -> InfNum a -> InfNum a
infMax MinusInfinity x = x
infMax x MinusInfinity = x
infMax PlusInfinity _  = PlusInfinity
infMax _ PlusInfinity  = PlusInfinity
infMax (Number a) (Number b) = Number (max a b)

infMaximum :: (Foldable t, Ord a) => t a -> InfNum a
infMaximum xs = foldr (\x y -> infMax (Number x) y) MinusInfinity xs

--
-- Exercise 3.3
--

-- product using recursion
myProduct :: [Integer] -> Integer
myProduct []      = 1
myProduct (x:xs)  = x * (myProduct xs)

-- product using foldr
myProduct' :: [Integer] -> Integer
myProduct' = foldr (*) 1

-- all using recursion
myAll :: [Bool] -> Bool
myAll []        = True
myAll (b:bs)    = b && (myAll bs)

-- all using foldr
myAll' :: [Bool] -> Bool
myAll' = foldr (&&) True

-- minimumBy using recursion
-- minimumBY applies f to each element before comparing, and returns the smallest
minimumBy :: (Ord a) => (a -> a) -> [a] -> a
minimumBy _ []          = error "Empty list"
minimumBy f (x:xs) = minBy f x xs
    where 
        minBy f acc []  = acc
        minBy f acc (x:xs) = if (f x) < (f acc)
                             then minBy f x xs
                             else minBy f acc xs

-- minimumBy using foldr
minimumBy' :: (Ord b) => (a -> b) -> [a] -> a
minimumBy' f (x:xs) = foldl minBy' x xs
    where
        minBy' m n = if (f m) < (f n) then m else n

-- minimumClient
minimumClient :: [Client] -> Client
minimumClient = minimumBy' (length . clientName)

--
-- Implement list functions
--

-- Non-naive implementation of bothFilters (i.e., partition)

bothFilters :: (a -> Bool) -> [a] -> ([a], [a])
bothFilters p lst = foldr bothFilters' ([],[]) lst
    where
        bothFilters' x (ts, fs)
            | p x           = (x:ts, fs)
            | otherwise     = (ts, x:fs)

-- myFind

myFind :: (a -> Bool) -> [a] -> Maybe a
myFind p [] = Nothing
myFind p (x:xs) =
    if p x then Just x else myFind p xs
 

