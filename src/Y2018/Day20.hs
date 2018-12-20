{-# LANGUAGE LambdaCase #-}

module Y2018.Day20 (answer1, answer2) where

import Control.Monad
import Data.Void

data Dir = N | S | W | E deriving (Show, Eq)
data Regex = Regex [Dir] [Regex] [Dir] deriving Show

answer1, answer2 :: IO ()
answer1 = print "wip 1"
answer2 = print "wip 2"

getData :: IO Regex
getData = buildRegex <$> readFile "data/2018/day20.txt"

fromChr :: Char -> Dir
fromChr = \case
  'N' -> N
  'S' -> S
  'W' -> W
  'E' -> E
  c -> error $ "cannot parse " <> [c]

buildRegex :: String -> Regex
buildRegex = head . fst . go [] [] []
  where
    go :: [Dir] -> [Regex] -> [Dir] -> String -> ([Regex], String)
    go _ _ _ [] = error "no end?"
    go l mid acc (c:cs) = case c of
      '^' -> go l mid acc cs
      '$' -> ([Regex l mid (reverse acc)], cs)
      '(' -> let (mid, remain) = go [] [] [] cs
              in go (reverse acc) mid [] remain
      '|' -> let (rest, remain) = go [] [] [] cs
              in (Regex l mid (reverse acc) : rest, remain)
      ')' -> ([Regex l mid (reverse acc)], cs)
      _ -> go l mid (fromChr c : acc) cs


test :: IO ()
test = do
  let r = "^ENWWW(NEEE|SSE(EE|N))$"
  print $ buildRegex r
