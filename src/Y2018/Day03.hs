{-# LANGUAGE OverloadedStrings #-}

module Y2018.Day03 (answer1, answer2) where

import           Control.Monad
import           Control.Monad.Loops

import qualified Data.IntSet                   as Set
import           Data.Maybe
import           Data.Functor
import           Data.Void
import           Data.Array                    as Arr

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

type Parser = Parsec Void Tx.Text

type Id = Int
data Rect = Rect
  { coord :: ((Int, Int), (Int, Int))
  , rId :: Id
  }
  deriving (Show, Eq)

answer1, answer2 :: IO ()
answer1 = do
  rects <- getData
  let grid = claimMap rects
  let res = filter ((>1) . length) (Arr.elems grid)
  print $ length res

answer2 = do
  rects <- getData
  -- print $ length $ filter (not . doesOverlap rects) rects

  let grid = claimMap rects
  let overlappingIds = Set.fromList
        $ concat
        $ filter ((>1) . length)
        (Arr.elems grid)
  let allIds = Set.fromList $ map rId rects

  case Set.toList (Set.difference allIds overlappingIds) of
    [x] -> print x
    _ -> error "bug"

gridBounds rects
  = let mx = maximum $ map (fst . snd . coord) rects
        my = maximum $ map (snd . snd . coord) rects
    in (mx+1, my+1)


claimMap rects =
  let vals = concatMap spots rects
      grid = accumArray (flip (:)) [] ((0,0), gridBounds rects) vals
  in grid

spots :: Rect -> [((Int, Int), Id)]
spots r =
  let ((x0, y0), (x1, y1)) = coord r
  in  [((x,y), rId r) | x <- [x0..x1], y <- [y0..y1]]

getData :: IO [Rect]
getData = do
  raw <- Tx.IO.readFile "data/2018/day03.txt"
  case parse dataParser "day03" raw of
    Left err -> error $ show err
    Right x -> pure x

dataParser :: Parser [Rect]
dataParser = parseLine `untilM` isEOF


parseLine :: Parser Rect
parseLine = do
  string "#"
  rId <- decimal
  string " @ "
  x0 <- decimal
  char ','
  y0 <- decimal
  string ": "
  w <- decimal
  char 'x'
  l <- decimal
  char '\n'
  pure $ Rect ((x0,y0), (x0+w-1, y0+l-1)) rId

isEOF :: Parser Bool
isEOF = (try eof $> True) <|> pure False


doesOverlap :: [Rect] -> Rect -> Bool
doesOverlap rects r = any (\r2 -> r2 /= r && overlap r r2 > 0) rects

overlap :: Rect -> Rect -> Int
overlap a b =
  let r1' = coord a
      r2' = coord b
      (r1, r2) = if r1' <= r2' then (r1', r2') else (r2', r1')
      ((x10, y10), (x11, y11)) = r1
      ((x20, y20), (x21, y21)) = r2
      i0 = max x10 x20
      j0 = max y10 y20
      i1 = min x11 x21
      j1 = min y11 y21
  in max 0 ((i1 - i0) * (j1 - j0))

test =
  [ Rect ((1,3), (4,6)) 1
  , Rect ((3,1), (6,4)) 2
  , Rect ((5,5), (6,6)) 3
  ]
