{-# LANGUAGE LambdaCase #-}

module Y2018.Day18 (answer1, answer2) where

import Control.Monad
import           Data.Array                     ( (!) )
import qualified Data.Array                    as A
import           Data.Maybe
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.List

data Area = Ground | Tree | Lumberyard
  deriving (Show, Eq, Ord)

type Point = (Int, Int)
type Grid = A.Array Point Area

answer1, answer2 :: IO ()
answer1 = do
  grid <- getData
  let allGrids = iterate nextGrid grid
  let target = allGrids !! 10
  print $ compute target

answer2 = do
  grid <- getData
  let allGrids = iterate nextGrid grid
  let (cycleStart, cycleLength, g) = findCycle allGrids
  let target = cycleStart + (1000000000 - cycleStart) `mod` cycleLength
  print $ compute $ allGrids !! target


findCycle :: [Grid] -> (Int, Int, Grid)
findCycle = go 0 mempty
  where
    go i seen (g:gs) = case M.lookup g seen of
      Nothing -> go (i+1) (M.insert g i seen) gs
      Just n -> (n, i-n, g)
    go _ _ _ = error "shut up ghc"


iterateMem :: Grid -> [Grid]
iterateMem = go 0 [] mempty
  where
    go i acc seen g
      | g `S.member` seen = acc
      | otherwise = let g' = nextGrid g
                        seen' = S.insert g seen
                    in go (i+1) (g':acc) seen' g'

compute :: Grid -> Int
compute g =
  let es = A.elems g
      nTree = length $ filter (== Tree) es
      nLumber = length $ filter (== Lumberyard) es
  in nTree * nLumber

nextGrid :: Grid -> Grid
nextGrid g =
  let elems = do
        y <- [minY..maxY]
        x <- [minX..maxX]
        pure ((x,y), nextSlot g (x,y))
  in A.array ((minX,minY), (maxX,maxY)) elems

nextSlot :: Grid -> Point -> Area
nextSlot g p@(x,y) =
  let around = do
        b <- [y-1..y+1]
        a <- [x-1..x+1]
        guard $ p /= (a,b)
        pure (a,b)
      areaAround = mapMaybe (get g) around
      nTree = length $ filter (== Tree) areaAround
      nLumber = length $ filter (== Lumberyard) areaAround
      c = g ! p
  in case c of
      Ground -> if nTree >= 3 then Tree else Ground
      Tree -> if nLumber >= 3 then Lumberyard else Tree
      Lumberyard -> if nLumber >= 1 && nTree >= 1 then Lumberyard else Ground

minX, minY, maxX, maxY :: Int
minX = 1
minY = 1
maxX = 50
maxY = 50

get :: Grid -> Point -> Maybe Area
get g p@(x,y) =
  let ((x0,y0),(x1,y1)) = A.bounds g
  in if (x0 <= x) && (x <= x1) && (y0 <= y) && (y <= y1)
      then Just $ g ! p
      else Nothing

getData :: IO Grid
getData = do
  raw <- readFile "data/2018/day18.txt"
  let ls = zip [1..] (lines raw)
  let elems = [((x,y), fromChar c) | (y,l) <- ls, (x,c) <- zip [1..] l]
  let grid = A.array ((minX,minY), (maxX, maxY)) elems
  pure grid

fromChar :: Char -> Area
fromChar = \case
  '.' -> Ground
  '|' -> Tree
  '#' -> Lumberyard
  c -> error $ "unknown symbol: " <> [c]

toChar :: Area -> Char
toChar = \case
  Ground -> '.'
  Tree -> '|'
  Lumberyard -> '#'

pretty :: Grid -> String
pretty g =
  let makeLine y = [toChar e | x <- [minX..maxX], let e = g ! (x,y)]
      ls = [makeLine y | y <- [minY..maxY]]
  in unlines ls

test :: IO ()
test = do
  g0 <- getData
  let grids = iterate nextGrid g0
  mapM_ (putStrLn . pretty) (take 3 grids)
