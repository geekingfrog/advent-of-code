module Day09 (answer1, answer2) where

import Control.Exception (throw)
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace
import Data.List (find, nub)

answer1 :: IO Int
answer1 = do
  vertices <- getData
  let allPaths = getPaths vertices
  return $ minimum $ map (sum . map third) allPaths

answer2 :: IO Int
answer2 = do
  vertices <- getData
  let allPaths = getPaths vertices
  return $ maximum $ map (sum . map third) allPaths

type City = String
type Distance = Int
type Vertex = (City, City, Distance)

select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]

getPaths vs = concatMap (\v -> getPaths' vs [first v] []) vs

getPaths' :: [Vertex] -> [City] -> [Vertex] -> [[Vertex]]
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

-- Parsing

getData :: IO [Vertex]
getData = do
  parsed <- parseFromFile verticesParser "./data/09.txt"
  case parsed of
    Left err -> throw err
    Right vertices -> return $ mirror vertices
  where mirror [] = []
        mirror (v@(start, end, d):vs) = v : (end, start, d) : mirror vs

verticesParser :: Parsec String [Vertex]
verticesParser = sepEndBy vertexParser newline

vertexParser = do
  start <- some letterChar
  string " to "
  end <- some letterChar
  string " = "
  d <- read <$> some digitChar
  return (start, end, d)
