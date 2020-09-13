{-# LANGUAGE InstanceSigs #-}

module Chapter6.Monads where

import Data.Maybe

-- Compute total purchases of a client
totalPurchase :: Integer -> Double
totalPurchase clientId =
    let p = purchasesByClientId clientId
    in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

-- Compute mean purchases of a client
meanPurchase :: Integer -> Double
meanPurchase clientId =
    let p = purchasesByClientId clientId
        n = fromIntegral $ length p
    in (foldr (+) 0.0 $ catMaybes $ map purchaseValue p) / n

{- -- Orignal version with cases
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId = 
    case numberItemsByPurchaseId purchaseId of
        Nothing -> Nothing
        Just n -> case productIdByPurchaseId purchaseId of
            Nothing -> Nothing
            Just prId -> case priceByProductId prId of
                Nothing -> Nothing
                Just price -> Just $ (fromInteger n) * price
 -}
{- 
-- Using our thenDo combinator
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
    numberItemsByPurchaseId purchaseId  `thenDo`   (\n ->
    productIdByPurchaseId purchaseId    `thenDo`  (\productId ->
    priceByProductId productId          `thenDo`   (\price ->
    Just $ fromInteger n * price        )))

thenDo :: Maybe a -> (a -> Maybe b) -> Maybe b
thenDo Nothing _ = Nothing
thenDo (Just x) f = f x
-}

-- Rewrite to use >>= and return
purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId =
    numberItemsByPurchaseId purchaseId  >>=   (\n ->
    productIdByPurchaseId purchaseId    >>=   (\productId ->
    priceByProductId productId          >>=   (\price ->
    return $ fromInteger n * price            )))

-- Dummy implementations of the database functions
purchasesByClientId :: Integer -> [Integer]
purchasesByClientId n = 
    case n of
        1 -> [1,2]
        2 -> [2]
        3 -> [1,2,3]
        4 -> [1,2,3,4]
        _ -> []

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId purchaseId =
    case purchaseId of
        1 -> Just 1
        2 -> Just 2
        3 -> Just 3
        _ -> Nothing

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId purchaseId =
    case purchaseId of
        1 -> Just 101
        2 -> Just 102
        3 -> Just 103
        _ -> Nothing

priceByProductId :: Integer -> Maybe Double
priceByProductId prId =
    case prId of
        101 -> Just 100.00
        102 -> Just 10.49
        103 -> Just 29.99
        _   -> Nothing


  -- Exercise 6.5 - Implement Writer monad
  
newtype MyWriter m a = MyWriter { runWriter :: (a, m) }

instance Functor (MyWriter m) where
    fmap f w = MyWriter $ let
        (x, m) = runWriter w
        in (f x, m)

instance Monoid m => Applicative (MyWriter m) where
    pure x = MyWriter (x, mempty)
    fw <*> w = MyWriter $ let
        (f, s) = runWriter fw
        (x, s') = runWriter w
        in (f x, s `mappend` s')

instance Monoid m => Monad (MyWriter m) where
    return = pure

    m >>= f = MyWriter $ let
        (x, s) = runWriter m
        (x', s') = runWriter (f x)
        in (x', s `mappend` s')

tell :: m -> MyWriter m ()
tell x = MyWriter ((), x)

example :: MyWriter [Int] String
example = do
    tell [1..3]
    tell [3..5]
    return "foo"

output :: (String, [Int])
output = runWriter example

example2 :: MyWriter [String] String
example2 = do
    tell ["Start"]
    let s1 = "foo"
    tell ["Process string 1"]
    let s2 = "bar"
    tell ["Process string 2"]
    tell ["End"]
    return $ s1 ++ " " ++ s2

output2 :: (String, [String])
output2 = runWriter example2