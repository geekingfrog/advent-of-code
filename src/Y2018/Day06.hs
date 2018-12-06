{-# LANGUAGE OverloadedStrings #-}

module Y2018.Day06 (answer1, answer2) where

import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Utils.Parser as Utils

import qualified Data.Array as Arr
import Control.Monad
import Data.Foldable
import Data.Void
import Data.Ord
import Data.List
import Data.Maybe

type Parser = Parsec Void Tx.Text
type Coord = (Int, Int)
type Grid = Arr.Array Coord (Maybe Int)
type Grid2 = Arr.Array Coord Bool


answer1, answer2 :: IO ()
answer1 = do
  coords <- getData
  let grid = makeGrid coords
  print $ maximum $ areas grid

answer2 = do
  coords <- getData
  print $ areas' $ makeGrid' coords 10000

makeGrid :: [Coord] -> Grid
makeGrid coords =
  let minX = minimum $ map fst coords
      minY = minimum $ map snd coords
      maxX = maximum $ map fst coords
      maxY = maximum $ map snd coords
      b = ((minX, minY), (maxX, maxY))
      withIdx = zip [1..] coords
      points = do
        x <- [minX..maxX]
        y <- [minY..maxY]
        let dists = sortOn snd $ map (\(i, c) -> (i, manhattan (x, y) c)) withIdx
        let v = case dists of
              (a:b:_) -> if snd a == snd b then Nothing else Just (fst a)
              _ -> Nothing
        pure ((x, y), v)
   in Arr.array b points


-- ignore infinite area, with any luck the biggest one is already inside
areas :: Grid -> [Int]
areas grid = map length $ group $ sort $ catMaybes $ Arr.elems grid

areas' :: Grid2 -> Int
areas' grid = length $ filter id $ Arr.elems grid

makeGrid' :: [Coord] -> Int -> Grid2
makeGrid' coords maxDist =
  let minX = minimum $ map fst coords
      minY = minimum $ map snd coords
      maxX = maximum $ map fst coords
      maxY = maximum $ map snd coords
      b = ((minX, minY), (maxX, maxY))
      withIdx = zip [1..] coords
      points = do
        x <- [minX..maxX]
        y <- [minY..maxY]
        let dist = sum $ map (manhattan (x,y)) coords
        pure ((x,y), dist < maxDist)
   in Arr.array b points


manhattan :: Coord -> Coord -> Int
manhattan (a, b) (c, d) = abs (c-a) + abs (d-b)

getData :: IO [Coord]
getData = do
  raw <- Tx.IO.readFile "data/2018/day06.txt"
  case parse gridParser "day06" raw of
    Left err -> error $ show err
    Right x -> pure x

gridParser :: Parser [Coord]
gridParser = Utils.parseLines $ (,) <$> decimal <* string ", " <*> decimal

test :: [Coord]
test =
  [ (1, 1)
  , (1, 6)
  , (8, 3)
  , (3, 4)
  , (5, 5)
  , (8, 9)
  ]
