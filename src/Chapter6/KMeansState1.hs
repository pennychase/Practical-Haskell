{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter6.KMeansState1 where

import Data.List
import qualified Data.Map as M

-- This version introduces using State with combinators
-- This is the most basic version, implementing state as a function s -> (a, s)


-- Define typeclass for Vector
-- Vectors must define a distance function and
-- to support KMeans, the centroid of a list of
-- vectors
class Ord v => Vector v where
    distance :: v -> v -> Double
    centroid :: [v] -> v

-- Define tuple instance
-- Requires FlexibleInstances
instance Vector (Double, Double) where
    distance (a, b) (c, d) = sqrt $ (c - a)*(c - a) + (d - b) * (d - b)
    centroid vs = 
        let (x, y) = foldr (\(a, b) (c, d) -> (a+c, b+d)) (0, 0) vs
            n = fromIntegral $ length vs
        in ( x / n, y / n)

-- Define typeclass for things that can be made into vectors
-- Requires MultiParamTypeCLasses
class Vector v => Vectorizable e v where
    toVector :: e -> v

-- instance for Double pairs
instance Vectorizable (Double, Double) (Double, Double) where
    toVector = id


-- KMeans using a State with combinators

type State s a = s -> (a, s)

thenDoS :: State s a -> (a -> State s b) -> State s b
thenDoS f g s = let (resultOfF, stateAfterF) = f s
                in g resultOfF stateAfterF

data KMeansStateS v = KMeansStateS { centroids :: [v]
                                   , threshold :: Double
                                   , steps :: Int
                                   }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
        in  foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                             in M.adjust (p:) chosenC m)
                  initialMap
                  points
    where
        compareDistance p x y = compare (distance x $ toVector p)
                                        ( distance y $ toVector p)

kMeansS' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansStateS v) [v]
kMeansS' points =
    -- injects current centroids
    (\s -> (centroids s, s))                                        `thenDoS` (\prevCentroids ->
    -- assign points to clusters
    (\s -> (clusterAssignments prevCentroids points, s))            `thenDoS` (\assignments ->
    -- create new centroids for the clusters
    (\s -> (newCentroids assignments, s))                           `thenDoS` (\newCentroids ->
    -- update the state
    (\s -> ( (), s { centroids = newCentroids }))                   `thenDoS` (\_ ->
    (\s -> ( (), s { steps = (steps s) + 1 }))                      `thenDoS` (\_ ->
    -- compare error against threshold
    (\s -> ( threshold s, s ))                                      `thenDoS` (\t ->
    (\s -> (sum $ zipWith distance prevCentroids newCentroids, s))  `thenDoS`
    (\err -> if err < t
             then (\s -> (newCentroids, s))
             else (kMeansS' points))))))))

-- Rewrite using combinators to make the state implicit

-- State remains the same and the value changes
remain :: a -> State s a
remain x = \s -> (x, s)

-- apply accessor function to state to return the value of the element
access :: (s -> a) -> State s a
access f = \s -> (f s, s)

-- Change the state
modify :: (s -> s) -> State s ()
modify f = \s -> ((), f s)

kMeansS'' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansStateS v) [v]
kMeansS'' points =
   -- injects current centroids
    access centroids                                                `thenDoS` (\prevCentroids ->
    -- assign points to clusters
    remain (clusterAssignments prevCentroids points)                `thenDoS` (\assignments ->
    -- create new centroids for the clusters
    remain (newCentroids assignments)                               `thenDoS` (\newCentroids ->
    -- update the state
    modify (\s -> s { centroids = newCentroids })                   `thenDoS` (\_ ->
    modify (\s -> s { steps = (steps s) + 1 })                      `thenDoS` (\_ ->
    -- compare error against threshold
    access threshold                                                `thenDoS` (\t ->
    remain (sum $ zipWith distance prevCentroids newCentroids)      `thenDoS` (\err ->
        if err < t
        then (\s -> (newCentroids, s))
        else (kMeansS' points))))))))

initialState :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateS v
initialState i k pts t = KMeansStateS (i k pts) t 0

kMeansS :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> [v]
kMeansS i k pts t = fst $ kMeansS'' pts (initialState i k pts t)


-- Generate vectors for testing
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
-- Usage:
-- info = [(1,1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
-- kMeansS initializeSimple 2 info 0.002
