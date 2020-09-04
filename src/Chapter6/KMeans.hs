{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module KMeans where

import Data.List
import qualified Data.Map as M

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

-- Create Map to hold the cluster assignments
-- For each point, find the closest centroid and add the point to that cluster
clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
    let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                      in M.adjust (p:) chosenC m)
              initialMap
              points
        where
            compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)

-- Compute new centroids by applying the centroid function to each point in the cluster
-- Need to turn point into a vector in order to use the cenrtoid function
newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

-- Stop computing when the change is below a threshold
shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
    foldr (\(x, y) s -> s + distance x y) 0.0 centroids < threshold

-- kMeans assigns points to clusters based on distance from centroid
-- clusterAssignmentPhase uses a Map to hold the cluster assignments
-- with centroid as key
-- newCentroidPhase computes the new centroids after points have been assigned to the clusters
-- The algorithm will converge, but shouldStop terminates once the change is below a
-- specified threshold

kMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v])   -- initialization function
       ->  Int                  -- number of centroids (clusters)
       -> [e]                   -- the data we're clustering
       -> Double                -- threshold
       -> [v]                   -- the final centroids
kMeans i k points = kMeans' (i k points) points
    where
        kMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> [v]
        kMeans' centroids points threshold =
            let assignments     = clusterAssignmentPhase centroids points
                oldNewCentroids = newCentroidPhase assignments
                newCentroids    = map snd oldNewCentroids
            in if shouldStop oldNewCentroids threshold
            then newCentroids
            else kMeans' newCentroids points threshold

-- Count the number of iterations and return a tuple with the number of iterations and the centroids
pkMeans :: (Vector v, Vectorizable e v)
       => (Int -> [e] -> [v])   -- initialization function
       ->  Int                  -- number of centroids (clusters)
       -> [e]                   -- the data we're clustering
       -> Double                -- threshold
       -> (Integer, [v])        -- the final centroids
pkMeans i k points threshold = pkMeans' (i k points) points threshold 0
    where
        pkMeans' :: (Vector v, Vectorizable e v) => [v] -> [e] -> Double -> Integer -> (Integer, [v])
        pkMeans' centroids points threshold steps =
            let assignments     = clusterAssignmentPhase centroids points
                oldNewCentroids = newCentroidPhase assignments
                newCentroids    = map snd oldNewCentroids
            in if shouldStop oldNewCentroids threshold
            then (steps, newCentroids)
            else pkMeans' newCentroids points threshold (steps + 1)


-- Generate vectors for testing
initializeSimple :: Int -> [e] -> [(Double, Double)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v

-- Usage:
-- info = [(1,1), (1, 2), (4, 4), (4, 5)] :: [(Double, Double)]
-- kMeans initializeSimple 2 info 0.002

