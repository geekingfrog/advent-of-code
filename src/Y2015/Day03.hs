module Y2015.Day03 (answer1, answer2) where

import Control.Monad (fmap)
import Data.List (scanl, nub)

type Pos = (Int, Int)


answer1 :: IO ()
answer1 = do
    directions <- getData
    let coords = scanl move (0, 0) directions
    print $ length . nub $ coords

answer2 :: IO ()
answer2 = do
    directions <- getData
    let santaDirections = takeEven directions
    let robotDirections = takeOdd directions
    let santaHouses     = scanl move (0, 0) santaDirections
    let robotHouses     = scanl move (0, 0) robotDirections
    print . length . nub $ santaHouses ++ robotHouses

getData = fmap (filter (/='\n')) (readFile "./data/03.txt")

move :: Pos -> Char -> Pos
move (x, y) 'v' = (x, y - 1)
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move _      _   = undefined

takeEven [] = []
takeEven l  = go l True
  where
    go []     _     = []
    go (x:xs) True  = x : go xs False
    go (x:xs) False = go xs True

takeOdd [] = []
takeOdd l  = takeEven (tail l)
