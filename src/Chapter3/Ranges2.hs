{-# LANGUAGE PatternSynonyms #-}

module Chapter3.Ranges2 (Range (R)) where

-- Range data type
data Range = Range Integer Integer deriving Show

-- smart constructor that performs check first
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

-- Pattern Synonym
pattern R :: Integer -> Integer -> Range
pattern R a b <- Range a b
    where R a b = range a b
