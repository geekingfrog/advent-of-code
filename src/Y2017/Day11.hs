{-# LANGUAGE OverloadedStrings #-}

module Y2017.Day11 (answer1, answer2) where

import Data.Functor
import Data.Void
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Data.Foldable

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Parser = Parsec Void T.Text

answer1, answer2 :: IO ()
answer1 = do
    dirs <- parseDirections <$> T.readFile "data/2017/day11.txt"
    let start = (0, 0, 0)
    let final = foldl' walk start dirs
    print $ dist final start
answer2 = do
    dirs <- parseDirections <$> T.readFile "data/2017/day11.txt"
    let start = (0, 0, 0)
    let all = scanl walk start dirs
    print $ maximum $ fmap (dist start) all

type Pos = (Int, Int, Int)
data Dir = N | NE | SE | S | SW | NW deriving (Eq, Show)

walk :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
walk (x, y, z) dir = case dir of
    N  -> (x, y + 1, z - 1)
    NE -> (x + 1, y, z - 1)
    SE -> (x + 1, y - 1, z)
    S  -> (x, y - 1, z + 1)
    SW -> (x - 1, y, z + 1)
    NW -> (x - 1, y + 1, z)


dist :: Pos -> Pos -> Int
dist (ax, ay, az) (bx, by, bz) = maximum [abs (ax - bx), abs (ay - by), abs (az - bz)]


parseDirections :: T.Text -> [Dir]
parseDirections raw = case parse directions "directions" raw of
    Left err -> error $ show err
    Right x -> x

directions :: Parser [Dir]
directions = dirParser `sepBy` char ','
  where
    dirParser =
        try (string "ne" $> NE)
            <|> try (string "nw" $> NW)
            <|> try (string "n" $> N)
            <|> try (string "sw" $> SW)
            <|> try (string "se" $> SE)
            <|> (string "s" $> S)
