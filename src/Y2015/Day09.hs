module Y2015.Day09 (answer1, answer2) where

import Control.Exception (throw)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Debug.Trace
import Data.List (find, nub)

import Tsp

type Parser = Parsec Void String

answer1 :: IO Int
answer1 = fst . minCycle <$> getData

answer2 :: IO Int
answer2 = fst . maxCycle <$> getData

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
