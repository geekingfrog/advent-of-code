{-# LANGUAGE ScopedTypeVariables #-}

module Y2018.Day11 (answer1, answer2) where

import Control.Monad
import Data.List
import Data.Function
import Data.Ord
import Data.Array
import qualified Data.Vector as V

answer1, answer2 :: IO ()
answer1 = print $ solve1 5535
answer2 = print $ solve2 5535

type Point = (Int, Int)
type Grid = Array Point Int

solve1 :: Int -> Point
solve1 sn = fst $ solveSize (makeGrid sn) 2

solve2 :: Int -> (Point, Int)
solve2 sn =
  let grid = makeGrid sn
      ms = [ ((x, y), s, t)
           | x <- [1..300]
           , y <- [1..300]
           , let (s, t) = solveCoord grid (x, y)
           ]
      third (_, _, x) = x
      (p, s, _) = maximumBy (comparing third) ms
   in (p, s)

solveSize :: Grid -> Int -> (Point, Int)
solveSize grid squareSize =
  let squares = do
        x0 <- [1..300 - squareSize]
        let x1 = x0 + squareSize
        y0 <- [1..300 - squareSize]
        let y1 = y0 + squareSize

        let total = sum $ do
              x <- [x0..x1]
              y <- [y0..y1]
              pure $ grid ! (x,y)
        pure ((x0, y0), total)
      m = maximumBy (\x y -> compare (snd x) (snd y)) squares
  in m

-- | given a grid of powerlevel and a starting point, return the
-- size of the grid with the highest total along with the total
solveCoord :: Grid -> Point -> (Int, Int)
solveCoord grid (x0, y0) =
  let maxSize = 300 - max x0 y0 + 1
      v = V.generate maxSize go
      go s | s == 0 = grid ! (x0, y0)
           | otherwise
             = v V.! (s-1)
             + sum [grid ! (x, y0+s) | x <- [x0..x0+s]]
             + sum [grid ! (x0+s, y) | y <- [y0..y0+s]]
             - grid ! (x0+s, y0+s)
      mi = V.maxIndex v
   in (mi+1, v V.! mi)

makeGrid :: Int -> Grid
makeGrid sn =
  let b = ((1,1), (300, 300))
      vals = do
        x <- [1..300]
        y <- [1..300]
        pure $ powerLevel sn (x, y)
  in listArray b vals

powerLevel :: Int -> Point -> Int
powerLevel sn (x, y) =
  let rack = x + 10
      p0 = rack * y + sn
      p1 = p0 * rack
      p2 = hundreds p1 - 5
  in p2

hundreds :: Int -> Int
hundreds n = (n `div` 100) `mod` 10
