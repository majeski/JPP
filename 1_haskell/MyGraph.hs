module MyGraph
( graph
, inGraph
, dfs
) where

import MyArray

data Graph = Graph (Int, Int) (Array Int [Int])

graph :: [(Int, [Int])] -> Graph
graph edges = Graph r (array r edges) where
  r = (minNode, maxNode)
  minNode = chooseNode minimum
  maxNode = chooseNode maximum
  chooseNode f = f . map f $ map (uncurry (:)) edges

inGraph :: Graph -> Int -> Bool
inGraph (Graph r _) = inRange r

dfs :: Graph -> Int -> [Int]
dfs g@(Graph r _) src
  | inGraph g src = elems $ dfs' g (array r []) src
  | otherwise = error "incorrect node"

dfs' :: Graph -> Array Int Int -> Int -> Array Int Int
dfs' g@(Graph _ nodes) visited cur
  | contains visited cur = visited
  | not $ contains nodes cur = visited'
  | otherwise = foldl (dfs' g) visited' (nodes ! cur)
  where
    visited' = update cur cur visited