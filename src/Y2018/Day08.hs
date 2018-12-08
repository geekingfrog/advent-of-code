module Y2018.Day08 (answer1, answer2) where

import Control.Monad.State as St
import Control.Monad
import qualified Data.Vector as V

import Data.Monoid

answer1, answer2 :: IO ()
answer1 = do
  tape <- getData
  print $ sumMeta $ buildTree tape
answer2 = nodeVal . buildTree <$> getData >>= print

data Node = Node
  { nMeta :: V.Vector Int
  , nChildren :: V.Vector Node
  }
  deriving (Show)

buildTree :: V.Vector Int -> Node
buildTree = St.evalState step

step :: St.State (V.Vector Int) Node
step = do
  s <- St.get
  if V.length s < 2
    then error "invalid struct?"
    else do
      let (a, xs) = V.splitAt 2 s
      let [c, m] = V.toList a
      St.put xs
      children <- V.replicateM c step
      (meta, remaining) <- St.gets (V.splitAt m)
      St.put remaining
      pure $ Node meta children

sumMeta :: Node -> Int
sumMeta n = V.sum (nMeta n) + sum (sumMeta <$> nChildren n)

nodeVal :: Node -> Int
nodeVal n
  | null (nChildren n) = sum (nMeta n)
  | otherwise
    = let cs = nChildren n
          res = fmap (maybe 0 nodeVal . (V.!?) cs . subtract 1) (nMeta n)
      in V.sum res

getData :: IO (V.Vector Int)
getData = V.fromList . map read . words <$> readFile "./data/2018/day08.txt"

test :: V.Vector Int
test = V.fromList $ map read $ words "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
