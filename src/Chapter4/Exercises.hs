{-# LANGUAGE LambdaCase #-}

module Chapter4.Exercises where

import qualified Data.Map as M

-- Write Map library functions in terms of M.alter

insert' :: Ord k => k -> a -> M.Map k a -> M.Map k a
insert' k newVal m = M.alter (\case _ -> (Just newVal)) k  m

delete' :: Ord k => k -> M.Map k a -> M.Map k a
delete' k m = M.alter (\case _ -> Nothing) k m

adjust' :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
adjust' f k m = M.alter (\case (Just x) -> (Just (f x)); Nothing -> Nothing) k m