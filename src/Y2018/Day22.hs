module Y2018.Day22 (answer1, answer2) where

import qualified Data.Array as A
import           Data.Array (Array, (!))
import GHC.Word



import Control.Monad
import qualified Data.Set as S
import Data.List

import Debug.Trace

answer1, answer2 :: IO ()
answer1 = do
  let depth = 3066
  let t = 13726
  let a = erosions depth t
  print $ sum [risk (a ! (x,y)) | y <- [0..t], x <- [0..t]]
  print "wip1"

answer2 = print "wip2"

type Coord = (Word32, Word32)

erosions :: Word32 -> Word32 -> Array Coord Word32
erosions depth target =
  let a = A.listArray ((0, 0), (target, target)) elems
      erosion x = (x + depth) `mod` 20183
      elems = [go x y | x <- [0..target], y <- [0..target]]
      -- elems = fmap (uncurry go) $ dedup $ do
      --   k <- [0..target]
      --   x <- [k..target]
      --   [(x,k), (k,x)]
      go x y | x == 0 && y == 0 = erosion 0
             | x == target && y == target = erosion 0
             | y == 0 = erosion (x * 16807)
             | x == 0 = erosion (y * 48271)
             | otherwise = erosion $ (a ! (x-1,y)) * (a ! (x,y-1))
  in a

risk i = i `mod` 3

dedup :: Ord a => [a] -> [a]
dedup = go mempty
  where
    go seen [] = []
    go seen (x:xs) = if S.member x seen then go seen xs else x : go (S.insert x seen) xs

test :: IO ()
test = do
  let d = 510
  let t = 10
  let a = erosions d t
  print $ sum [risk (a ! (x,y)) | y <- [0..t], x <- [0..t]]

  -- print $ sort $ dedup $ do
  --   k <- [0..2]
  --   x <- [k..2]
  --   [(x,k), (k,x)]
  --   -- y <- [0..3]
  --   -- guard $ x /= y
  --   -- [(x,0), (0,y)]

  print "done"
