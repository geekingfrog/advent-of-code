{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day10 (answer1, answer2) where

import           Control.Monad
import           Data.Foldable
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO
import           Data.Void
import qualified Data.Vector                   as V
import Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Array                    as Arr

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
  forM_ (zip [1..] [allPos !! 100]) $ \(i, p) -> do
    Tx.IO.putStrLn $ Tx.pack $ show i
    Tx.IO.putStr (pretty p)
    Tx.IO.putStr "\n"
answer2 = print "wip2"


infixr 5 .+.
(.+.) :: Point -> Point -> Point
(a, b) .+. (c, d) = (a+c, b+d)

move :: V.Vector Point -> V.Vector Point -> V.Vector Point
move = V.zipWith (.+.)

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
