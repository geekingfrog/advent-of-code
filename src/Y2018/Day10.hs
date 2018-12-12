{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day10 (answer1, answer2) where

import           Control.Monad
import           Data.Maybe
import           Data.Foldable
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO
import           Data.Void
import qualified Data.Vector                   as V
import           Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Array                    as Arr
import qualified Control.Foldl                 as L

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import qualified Utils.Parser as Utils

type Parser = Parsec Void Tx.Text
type Point = (Int, Int)

answer1, answer2 :: IO ()
answer1 = do
  (ps, vs) <- getData
  let allPos = iterate (move vs) ps
  let (Just x) = find shouldPrint allPos
  Tx.IO.putStrLn (pretty x)

answer2 = do
  (ps, vs) <- getData
  let allPos = iterate (move vs) ps
  let withIdx = zip [0..] allPos
  let (Just (i, x)) = find (shouldPrint . snd) withIdx
  print i


infixr 5 .+.
(.+.) :: Point -> Point -> Point
(a, b) .+. (c, d) = (a+c, b+d)

move :: V.Vector Point -> V.Vector Point -> V.Vector Point
move = V.zipWith (.+.)

shouldPrint :: V.Vector Point -> Bool
shouldPrint v =
  let (minY, maxY) = boundsY v
  in abs (maxY - minY) <= 15

pretty :: V.Vector Point -> Tx.Text
pretty points =
  let (minX, minY) = (V.minimum (fmap fst points), V.minimum (fmap snd points))
      (maxX, maxY) = (V.maximum (fmap fst points), V.maximum (fmap snd points))
      assocs = V.toList (fmap (, '#') points)
      a = Arr.accumArray (\_ _ -> '#') ' ' ((minX, minY), (maxX, maxY)) assocs
      m = Map.fromList (V.toList $ fmap (, '#') points)
      line y = Tx.pack [(Arr.!) a (x, y) | x <- [minX..maxX]]
      lines = [line y | y <- [minY..maxY]]
   in Tx.unlines lines

boundsX, boundsY :: V.Vector Point -> (Int, Int)
boundsX = L.fold (minMax fst)
boundsY = L.fold (minMax snd)

minMax f
  = (,)
  <$> fmap fromJust (L.premap f L.minimum)
  <*> fmap fromJust (L.premap f L.maximum)

getData :: IO (V.Vector Point, V.Vector Point)
getData = do
  raw <- Tx.IO.readFile "data/2018/day10.txt"
  case parse dataParser "day10" raw of
    Right x -> pure $ V.unzip (V.fromList x)
    Left err -> error $ show err

dataParser = Utils.parseLines lineParser

lineParser :: Parser (Point, Point)
lineParser = do
  string "position=<"
  x <- num
  string ", "
  y <- num
  string "> velocity=<"
  vx <- num
  string ", "
  vy <- num
  char '>'
  pure ((x, y), (vx, vy))

  where
    num = space *> ((char '-' $> negate) <*> decimal <|> decimal)
