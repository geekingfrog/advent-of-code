module Tsp (minCycle, maxCycle, Node, Cost, Vertex) where

import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

type Node = String
type Cost = Int
type Vertex = (Node, Node, Cost)

getPaths :: [Vertex] -> [[Vertex]]
getPaths vs = concatMap (\v -> getPaths' vs [first v] []) vs

getPaths' :: [Vertex] -> [Node] -> [Vertex] -> [[Vertex]]
getPaths' vertices visited acc = paths
  where
    prev = head visited
    validVertices = filter (\(start, end, _) -> start == prev && notElem end visited) vertices
    paths = if null validVertices
            then [reverse acc]
            else concatMap (\v -> getPaths' vertices (second v:visited) (v:acc)) validVertices

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

minCycle :: [Vertex] -> (Cost, [Vertex])
minCycle vs = let
  paths = getPaths vs
  withCosts = map (\(p1, p2) -> (sum $ map third p1, p2)) (zip paths paths)
  in minimumBy (comparing fst) withCosts

maxCycle :: [Vertex] -> (Cost, [Vertex])
maxCycle vs = let
  paths = getPaths vs
  withCosts = map (\(p1, p2) -> (sum $ map third p1, p2)) (zip paths paths)
  in maximumBy (comparing fst) withCosts
