{-# LANGUAGE OverloadedStrings #-}

module Y2018.Day12 (answer1, answer2) where

import Control.Monad
import Data.Maybe
import Data.Bool
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Vector as V
import Data.Vector ((!), (!?))
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO
import qualified Utils.Parser as U
import qualified Data.Map.Strict as Map
import Data.Void
import Data.Functor
import Data.Bits as B


import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type Parser = Parsec Void Tx.Text

type PlantState = IntSet
type Rules = IntSet

answer1, answer2 :: IO ()
answer1 = do
  (s0, rules) <- getData
  print $ solve rules s0 20

answer2 = do
  (s0, rules) <- getData
  let allStates = iterate (genStep rules) s0
  -- assume stable pattern before 200 gen
  let finalLength = lengthState (allStates !! 200)
  let (Just (i, _)) = find (\(_, s) -> lengthState s == finalLength) (zip [0..] allStates)
  let target = 50000000000
  let n = target - i
  let res = IntSet.foldl' (\acc k -> acc + k + n) 0 (allStates !! i)
  print res

lengthState s = IntSet.findMax s - IntSet.findMin s

solve :: Rules -> PlantState -> Int -> Int
solve rules s0 n =
  let allStates = iterate (genStep rules) s0
      res = IntSet.foldl' (+) 0 (allStates !! n)
   in res

genStep :: Rules -> PlantState -> PlantState
genStep rules s =
  let ks = concatMap (\x -> [x-2..x+2]) (IntSet.elems s)
      new = filter (stepPlant rules s) ks -- mapMaybe (stepPlant rules s) ks
   in IntSet.fromList new

stepPlant :: Rules -> PlantState -> Int -> Bool
stepPlant rules s idx =
  let d0 = bool 0 16 $ (idx - 2) `IntSet.member` s
      d1 = bool 0 8  $ (idx - 1) `IntSet.member` s
      d2 = bool 0 4  $  idx      `IntSet.member` s
      d3 = bool 0 2  $ (idx + 1) `IntSet.member` s
      d4 = bool 0 1  $ (idx + 2) `IntSet.member` s
      key = d0+d1+d2+d3+d4
   in key `IntSet.member` rules

getData :: IO (PlantState, Rules)
getData = do
  raw <- Tx.IO.readFile "data/2018/day12.txt"
  case parse dataParser "day12" raw of
    Right x -> pure x
    Left err -> error $ show err

dataParser :: Parser (PlantState, Rules)
dataParser = do
  s0 <- parseInitialState
  newline
  rules <- parseRules
  pure (s0, rules)


parseInitialState = do
  string "initial state: "
  states <- U.parseLine parsePot
  let plants = filter ((==1) . snd) (zip [0..] states)
  pure $ IntSet.fromList $ fmap fst plants

parseRules = do
  raws <- U.parseLines parseRule
  pure $ IntSet.fromList $ catMaybes raws

parseRule = do
  d0 <- (*16) <$> parsePot
  d1 <- (*8)  <$> parsePot
  d2 <- (*4)  <$> parsePot
  d3 <- (*2)  <$> parsePot
  d4 <- (*1)  <$> parsePot
  string " => "
  res <- parsePot
  if res == 0
     then pure Nothing
     else pure (Just $ d0+d1+d2+d3+d4)

parsePot :: Parser Int
parsePot
  = char '#' $> 1
  <|> char '.' $> 0
