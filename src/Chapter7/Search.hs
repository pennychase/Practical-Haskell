module Chapter7.Search where

import Control.Monad
import Control.Monad.Logic

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

-- Using Logic Monad
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


-- Examples graphs
graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558), (1004, 2558)]

-- graph with cycle
graph2 :: [(Int, Int)]
graph2 = [(2013, 501), (501, 2558), (501, 1004), (1004, 501), (2013, 2558)]




