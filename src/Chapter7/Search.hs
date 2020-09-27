{-# LANGUAGE FlexibleContexts #-}
module Chapter7.Search where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS

-- Find paths between two points in a graph using lists with MonadPlus

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end = 
    let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- paths edges e_end end
        return $ start:subpath
    in if start == end
        then return [end] `mplus` e_paths
        else e_paths

--
-- Using Logic Monad
--

-- to see the results: 
-- observeAll $ pathsL graph2 2013 2558     -- this will print out infinite results for the graph with cycle
-- observeMany n $ pathsL graph2 2013 2558  -- show n results
pathsL :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL edges start end = 
    let e_paths = do
        (e_start, e_end) <- choices edges
        guard $ e_start == start
        subpath <- pathsL edges e_end end
        return $ start:subpath
    in if start == end
        then return [end] `mplus` e_paths
        else e_paths

-- Desugared
pathsL' :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsL' edges start end = 
    let e_paths = choices edges                 >>= (\(e_start, e_end) -> 
                  (guard $ e_start == start)    >>
                  pathsL' edges e_end end       >>= (\subpath -> 
                  return $ start:subpath))
    in if start == end
        then return [end] `mplus` e_paths
        else e_paths

-- Fair version
-- Replaces >>= with >>- (fair bind) and mplus with interleave (so the first arg alternates
-- with second arg when combining)
pathsLFair :: [(Int, Int)] -> Int -> Int -> Logic [Int]
pathsLFair edges start end =
    let e_paths = choices edges                 >>- \(e_start, e_end) ->
                  guard (e_start == start)      >>
                  pathsLFair edges e_end end    >>- \subpath ->
                  return $ start:subpath
    in if start == end
       then return [end] `interleave` e_paths
       else e_paths

choices :: [a] -> Logic a
choices = msum . map return

--
-- Using Monad Transformers wrapping WriterT with the List Monad
--

pathsWriterT' :: [(Int, Int)] -> Int -> Int -> WriterT [Int] [] ()
pathsWriterT' edges start end =
    let e_paths = do
        (e_start, e_end) <- lift edges
        guard $ e_start == start
        tell [start]
        pathsWriterT' edges e_end end
    in if start == end then tell [start] `mplus` e_paths else e_paths

pathsWriterT :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriterT edges start end = execWriterT (pathsWriterT' edges start end)

--
-- Using Monad Classes instead of transformer stack
--

type Graph = [(Int, Int)]

pathsImplicitStack:: (MonadReader Graph m, MonadWriter [Int] m, MonadPlus m) => Int -> Int -> m ()
pathsImplicitStack start end = 
    let e_paths = do
        (e_start, e_end) <- ask >>= msum . map return
        guard $ e_start == start
        tell [start]
        pathsImplicitStack e_end end
    in  if start == end then tell [end] else e_paths

pathsImplicitRW :: Graph -> Int -> Int -> [[Int]]
pathsImplicitRW edges start end = 
    execWriterT rdr
        where rdr = runReaderT (pathsImplicitStack start end) edges :: WriterT [Int] [] ()

pathsImplicitRWS :: Graph -> Int -> Int -> [[Int]]
pathsImplicitRWS edges start end = 
    map snd exec
        where exec = execRWST (pathsImplicitStack start end) edges ()

-- Example graphs
graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]

-- graph with cycle
graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 2558)]




