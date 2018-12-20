{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day17 (answer1, answer2) where

import           Data.Void
import           Data.Array                     ( (!) )
import qualified Data.Array                    as A
import           Data.List
import qualified Data.Set                      as S
import           Data.Maybe

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Utils.Parser                  as U

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

type Parser = Parsec Void Tx.Text

answer1, answer2 :: IO ()
answer1 = print "wip1"
answer2 = print "wip2"

type Point = (Int, Int)
type Grid = A.Array Point Bool


flowStep :: Grid -> [Point] -> S.Set Point -> (S.Set Point, [Point])
flowStep _ [] flooded = (flooded, [])
flowStep grid (s:ss) flooded
  = case flowDown grid flooded s of
      [] -> flowStep grid ss flooded
      xs@(x:_) ->
        let (sides, sources) = flowSides grid x
            flooded' = insertMany flooded (xs <> sides)
         in if null sources
               then flowStep grid ((x .+. (0,-1)) : ss) flooded'
               else flowStep grid (ss <> sources) flooded'


flowDown :: Grid -> S.Set Point -> Point -> [Point]
flowDown = go []
  where
    go acc grid flooded p
      | p `S.member` flooded = [p]
      | get grid p == Just False = go (p:acc) grid flooded (p .+. (0,1))
      | otherwise = acc


flowSides :: Grid -> Point -> ([Point], [Point])
flowSides grid p =
  let (l, mbSl) = flowSide (-1,0) grid p
      (r, mbSr) = flowSide ( 1,0) grid p
      newFlooded = l <> r
      newSources = catMaybes [mbSl, mbSr]
   in (newFlooded, newSources)

flowSide :: Point -> Grid -> Point -> ([Point], Maybe Point)
flowSide = go []
  where
    go acc dir grid p =
      let x = get grid p
          r | x == Just True || isNothing x = (acc, Nothing)
            | get grid (p .+. (0,1)) == Just False = (p:acc, Just p)
            | otherwise = go (p:acc) dir grid (p .+. dir)
       in r
      -- | get grid p == Just True = (acc, Nothing)


infixr 5 .+.
(.+.) :: Point -> Point -> Point
(a, b) .+. (c, d) = (a+c, b+d)


get :: Grid -> Point -> Maybe Bool
get g (x, y) =
  let ((x0, y0), (x1, y1)) = A.bounds g
   in if (x0 <= x) && (x <= x1) && (y0 <= y) && (y <= y1)
         then Just (g ! (x,y))
         else Nothing

insertMany :: S.Set Point -> [Point] -> S.Set Point
insertMany = foldl' (flip S.insert)

prettyGrid :: Grid -> S.Set Point -> Tx.Text
prettyGrid g flooded =
  let ((minX, minY), (maxX, maxY)) = A.bounds g
      makeRow y = Tx.pack [prettyChar (x,y) | x <- [minX..maxX]]
      prettyChar p
        | p == (500,0) = '+'
        | p `S.member` flooded = '~'
        | g ! p = '#'
        | otherwise = '.'
      res = Tx.unlines [makeRow y | y <- [minY..maxY]]
  in res

getData :: IO Grid
getData = do
  raw <- Tx.IO.readFile "data/2018/day17_test.txt"
  case parse gridParser "day17" raw of
    Left err -> error $ show err
    Right x -> pure x

gridParser :: Parser Grid
gridParser = do
  points <- concat <$> U.parseLines lineParser
  let minX = minimum (fmap fst points) - 1
  let minY = 0
  let maxX = maximum (fmap fst points) + 1
  let maxY = maximum (fmap snd points)
  let b = ((minX, minY), (maxX, maxY))
  pure $ A.accumArray (\_ _ -> True) False b (fmap (,True) points)

lineParser :: Parser [Point]
lineParser = do
  a <- char 'x' <|> char 'y'
  char '='
  n <- decimal
  string ", "
  b <- char 'x' <|> char 'y'
  char '='
  i <- decimal
  string ".."
  j <- decimal
  if a == 'x'
    then pure [(n, y) | y <- [i..j]]
    else pure [(x, n) | x <- [i..j]]

test :: IO ()
test = do
  grid <- getData
  print $ A.bounds grid
  let source = (499,0)
  -- let flooded = S.fromList $ flowDown grid mempty (source .+. (0,1))
  -- let (flooded', mbSource) = flowSide (1, 0) grid (499,7)
  -- let flooded = flow grid [source] mempty
  -- Tx.IO.putStr $ prettyGrid grid flooded
  print "done"
