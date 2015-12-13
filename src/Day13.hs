module Day13 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Exception (throw)

import Data.Maybe (fromMaybe)
import Data.List (find, nub)
import Control.Monad (liftM)
import Tsp (Vertex, Cost, maxCycle)

answer1 :: IO Int
answer1 = do
  vertices <- liftM (nub . adjustCosts) getData
  let optimal = optimalSitting vertices
  return $ sum $ map third optimal

answer2 :: IO Int
answer2 = do
  vertices <- liftM (nub . adjustCosts) getData
  let optimal = optimalSitting vertices
  let costs = map third optimal
  return $ sum costs - minimum costs

optimalSitting :: [Vertex] -> [Vertex]
optimalSitting vs = let
  sitting = snd $ maxCycle vs
  lastSitting = completeSitting sitting vs
  in lastSitting : sitting

completeSitting sitting vs = let
  firstPerson = first $ head sitting
  lastPerson = second $ last sitting
  missingSitting = find (\(a, b, _) -> b == firstPerson && a == lastPerson) vs
  err = "Cannot find sitting between " ++ show firstPerson ++ " and " ++ show lastPerson
  in fromMaybe (error err) missingSitting

first (a, _, _) = a
second (_, a, _) = a
third (_, _, a) = a

adjustCosts vs = map (adjustCost vs) vs

adjustCost :: [Vertex] -> Vertex -> Vertex
adjustCost vs (p1, p2, c) = let
  Just mirror = find (\(a, b, _) -> p1 == b && p2 == a) vs
  in (p1, p2, c + third mirror)


-- parsing

getData :: IO [Vertex]
getData = do
  raw <- parseFromFile relationsParser "./data/13.txt"
  case raw of
    Left err -> throw err
    Right dat -> return dat

relationsParser :: Parsec String [Vertex]
relationsParser = sepEndBy relationParser newline

relationParser = do
  n1 <- some letterChar
  string " would "
  mult <- (string "lose" >> return (-1)) <|> (string "gain" >> return 1)
  space
  happy <- read <$> some digitChar
  string " happiness units by sitting next to "
  n2 <- some letterChar
  char '.'
  return (n1, n2, mult * happy)
