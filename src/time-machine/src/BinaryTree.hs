module BinaryTree where

import TimeMachine
import Data.Monoid

-- Simple Binary Tree for Travel Guides

data BinaryTree1 = Node1 TravelGuide BinaryTree1 BinaryTree1
                 | Leaf1
                 deriving Show

treeFind1 :: TravelGuide -> BinaryTree1 -> Maybe TravelGuide
treeFind1 t (Node1 v l r) = 
    case compare t v of
        EQ -> Just v
        LT -> treeFind1 t l
        GT -> treeFind1 t r
treeFind1 _ Leaf1 = Nothing

treeInsert1 :: TravelGuide -> BinaryTree1 -> BinaryTree1
treeInsert1 t n@(Node1 v l r) =
    case compare t v of
        EQ -> n
        LT -> Node1 v (treeInsert1 t l) r
        GT -> Node1 v l (treeInsert1 t r)
treeInsert1 t Leaf1 = Node1 t Leaf1 Leaf1

-- Polymorphic Binary Tree

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a)
                   | Leaf2
                 deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 e (Node2 v l r) =
    case compare e v of
        EQ -> Just v
        LT -> treeFind2 e l
        GT -> treeFind2 e r
treeFind2 _ Leaf2 = Nothing

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 e t@(Node2 v l r) =
    case compare e v of
        EQ -> t
        LT -> Node2 v (treeInsert2 e l) r
        GT -> Node2 v l (treeInsert2 e r)
treeInsert2 e Leaf2 = Node2 e Leaf2 Leaf2

fromList2 :: Ord a => [a] -> BinaryTree2 a
fromList2 = foldr treeInsert2 Leaf2

flatten2 :: Ord a => BinaryTree2 a -> [a]
flatten2 (Node2 v l r) = concat [flatten2 l, [v], flatten2 r]
flatten2 Leaf2 = []

concatTree2 :: Ord a => [BinaryTree2 a] -> BinaryTree2 a
concatTree2 [] = Leaf2
concatTree2 (t:ts) = foldr treeInsert2 (concatTree2 ts) (flatten2 t) 

-- Polymorphic Binary Tree with Cache

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)

treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c (Node3 v' c' l r) =
    case compare v v' of
        EQ -> Node3 v c l r
        LT -> Node3 v' (min c c') (treeInsert3 v c l) r
        GT -> Node3 v' (min c c') l (treeInsert3 v c r)
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3

-- Polymorphic Binary Tree with Monoidal Cache

-- We reuse the BinaryTree3 data type, but the cache is now a monoid
treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v' c' l r) =
    case compare v v' of
        EQ -> Node3 v' c' l r
        LT -> let newLeft = treeInsert4 v c l
                  newCache =  c' <> cached newLeft <> cached r
              in Node3 v' newCache newLeft r
        GT -> let newRight = treeInsert4 v c r
                  newCache =  c' <> cached l <> cached newRight
              in Node3 v' newCache l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3           = mempty

-- create a monoid for min to see that the monoid version works

newtype Min = Min Double deriving Show

instance Semigroup Min where
    Min x <> Min y = Min $ min x y

instance Monoid Min where
    mempty = Min infinity where infinity = 1/0
    mappend = (<>)

-- Examples
-- Min
m1 = treeInsert4 10 (Min 10) Leaf3      -- Node 3 (Min 10.0) Leaf3 Leaf3
m2 = treeInsert4 5 (Min 5) m1           -- Node3 10 (Min 5.0) (Node3 5 (Min 5.0) Leaf3 Leaf3) Leaf3
m3 = treeInsert4 7 (Min 7) m2           -- Node3 10 (Min 5.0) (Node3 5 (Min 5.0) Leaf3 (Node3 7 (Min 7.0) Leaf3 Leaf3)) Leaf3
m4 = treeInsert4 3 (Min 3) m3           -- Node3 10 (Min 3.0) (Node3 5 (Min 3.0) (Node3 3 (Min 3.0) Leaf3 Leaf3) (Node3 7 (Min 7.0) Leaf3 Leaf3)) Leaf3
-- Sum
s1 = treeInsert4 10 (Sum 10) Leaf3      -- Node3 10 (Sum {getSum = 10}) Leaf3 Leaf3
s2 = treeInsert4 5 (Sum 5) s1           -- Node3 10 (Sum {getSum = 15}) (Node3 5 (Sum {getSum = 5}) Leaf3 Leaf3) Leaf3
s3 = treeInsert4 8 (Sum 8) s2           -- Node3 10 (Sum {getSum = 28}) (Node3 5 (Sum {getSum = 13}) Leaf3 (Node3 8 (Sum {getSum = 8}) Leaf3 Leaf3)) Leaf3
s4 = treeInsert4 6 (Sum 6) s3           -- Node3 10 (Sum {getSum = 55}) (Node3 5 (Sum {getSum = 27}) Leaf3 (Node3 8 (Sum {getSum = 14}) (Node3 6 (Sum {getSum = 6}) Leaf3 Leaf3) Leaf3)) Leaf3

-- Note that Sum doesn't really cache the sum. The way the insert function is written, summing will double count
-- from subtrees.

