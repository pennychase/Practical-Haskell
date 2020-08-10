{-# LANGUAGE ViewPatterns #-}

module Chapter3.Ranges (Range(), range, RangeView (R), r) where

-- Range data type
data Range = Range Integer Integer deriving Show

-- smart constructor that performs check first
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

-- Define a view for range, which will be exported
-- This is the view data type
data RangeView = R Integer Integer deriving Show

-- this is the View Pattern
r :: Range -> RangeView
r (Range a b) = R a b
