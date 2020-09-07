{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Chapter6.KMeansState2 where

import Data.List
import qualified Data.Map as M
import Control.Monad.State
    ( MonadState(put, get), gets, unless, modify, evalState, State )
import Control.Monad.RWS
import Control.Monad (unless)
import Data.Monoid (Sum(..))

-- This version uses the State Monad

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

-- KMeans State for State monad                   
data KMeansStateS v = KMeansStateS { centroids :: [v]
                                   , threshold :: Double
                                   , steps :: Int
                                   }

newCentroidAssignments :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroidAssignments = M.elems . fmap (centroid . map toVector)

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

-- State Monad using KMeansStateS to store the state
kMeansS' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansStateS v) [v]
kMeansS' points = do
    prevCentroids <- gets centroids
    let assignments = clusterAssignments prevCentroids points
        newCentroids = newCentroidAssignments assignments
    modify (\s -> s { centroids = newCentroids })
    modify (\s -> s { steps = steps s + 1})
    t <- fmap threshold get 
    let err = sum $ zipWith distance prevCentroids newCentroids
    if err < t then return newCentroids else kMeansS' points

initialState :: (Vector v, Vectorizable e v)
             => (Int -> [e] -> [v]) -> Int -> [e] -> Double -> KMeansStateS v
initialState i k pts t = KMeansStateS (i k pts) t 0

kMeansS :: (Vector v, Vectorizable e v) => (Int -> [e] -> [v])  -> Int -> [e] -> Double -> [v]
kMeansS i n pts t = evalState (kMeansS' pts) (initialState i n pts t)


-- RWS Monad
-- RWS has four arguments:
--  R - Double for holding the threshold (we only read)
--  W - (Sum Int) for holding the steps (we only update)
--  S - [v] for the centroids (we read and update)
kMeansRWS' :: (Vector v, Vectorizable e v) => [e] -> RWS Double (Sum Int) [v] ()
kMeansRWS' points =
    do
        prevCentroids <- get  
        let assignments = clusterAssignments prevCentroids points
            newCentroids = newCentroidAssignments assignments
        put newCentroids
        tell (Sum 1)
        t <- ask          
        let err = sum $ zipWith distance prevCentroids newCentroids
            in unless (err < t) $ kMeansRWS' points

kMeansRWS :: (Vector v, Vectorizable e v)
          => (Int -> [e] -> [v])            -- initialization function
          -> Int                            -- number of clusters
          -> [e]                            -- points
          -> Double                         -- threshold
          -> ([v], Sum Int)                 -- returns the clusters and number of steps
kMeansRWS i n pts t = execRWS (kMeansRWS' pts) t (i n pts)


-- Generate vectors for testing
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v
-- Usage:
-- info = [(1,1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
-- kMeansS initializeSimple 2 info 0.002
-- kMeansRWS initializeSimple 2 info 0.002
