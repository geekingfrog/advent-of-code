{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day17 (answer1, answer2) where

import           Data.Void
import           Data.Array                     ( (!) )
import qualified Data.Array                    as A
import           Data.List
import qualified Data.Set                      as S
import           Data.Maybe
import Control.Monad

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


flowStep :: Grid -> (S.Set Point, [Point]) -> (S.Set Point, [Point])
flowStep _ (flooded, []) = (flooded, [])
flowStep grid (flooded, s:ss)
  = case flowDown grid flooded (s .+. (0,1)) of
      [] -> (flooded, ss')
      xs@(x:_) ->
        let flooded1 = insertMany flooded xs
            (sides, sources) = flowSides grid flooded1 x
            flooded2 = insertMany flooded1 sides
         in if null sources
               then (flooded2, (x .+. (0,-1)) : ss')
               else (flooded2, ss <> sources)

  where ss' = nub ss


flowDown :: Grid -> S.Set Point -> Point -> [Point]
flowDown = go []
  where
    go acc grid flooded p
      | p `S.member` flooded = [p]
      | get grid p == Just False = go (p:acc) grid flooded (p .+. (0,1))
      | otherwise = acc


flowSides :: Grid -> S.Set Point -> Point -> ([Point], [Point])
flowSides grid flooded p =
  let (l, mbSl) = flowSide grid flooded (-1,0) p
      (r, mbSr) = flowSide grid flooded ( 1,0) p
      newFlooded = l <> r
      newSources = catMaybes [mbSl, mbSr]
   in (newFlooded, newSources)

-- | return (newly flooded points, and maybe a new 'source'
flowSide :: Grid -> S.Set Point -> Point -> Point -> ([Point], Maybe Point)
flowSide = go []
  where
    go acc grid flooded dir p =
      let x = get grid p
          r | x == Just True || isNothing x = (acc, Nothing)
            | get grid (p .+. (0,1)) == Just False
            && (p .+. (0,1)) `S.notMember` flooded = (p:acc, Just p)
            | otherwise = go (p:acc) grid flooded dir (p .+. dir)
       in r


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
        | p == (499,0) = '+'
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
  let source = (499,0)
  let allSteps = iterate (flowStep grid) (mempty, [source])

  forM_ (take 4 allSteps) $ \(flooded, sources) -> do
    Tx.IO.putStrLn $ prettyGrid grid flooded

  -- Tx.IO.putStrLn $ prettyGrid grid (S.fromList $ flowDown grid mempty (494,5))

  print "done"
