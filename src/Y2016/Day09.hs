module Y2016.Day09 (answer1, answer2) where

import Control.Exception (throw)
import Text.Megaparsec
import Text.Megaparsec.String
import Debug.Trace
import Data.List (find, nub)

import Tsp

answer1 :: IO Int
answer1 = do
    vertices <- getData
    return $ fst $ minCycle vertices

answer2 :: IO Int
answer2 = do
    vertices <- getData
    return $ fst $ maxCycle vertices

-- Parsing

getData :: IO [Vertex]
getData = do
    content <- readFile "./data/09.txt"
    case parse verticesParser "" content of
        Left  err      -> throw err
        Right vertices -> return $ mirror vertices
  where
    mirror []                     = []
    mirror (v@(start, end, d):vs) = v : (end, start, d) : mirror vs

verticesParser :: Parser [Vertex]
verticesParser = sepEndBy vertexParser newline

vertexParser = do
    start <- some letterChar
    string " to "
    end <- some letterChar
    string " = "
    d <- read <$> some digitChar
    return (start, end, d)
