module Day24 (answer1, answer2) where

import Data.List (nub, sortBy, minimumBy)
import Data.Ord (comparing)
import Control.Arrow (first)
import Control.Monad.Trans.State
import Control.Monad (guard)

-- answer1 :: Int
answer1 =
    let
        target = sum weights `quot` 3
        firstGroups =
            sortBy (comparing (length . fst)) $ selectTarget target weights
        validFirstGroups =
            filter (\(g, rs) -> not . null $ selectTarget target rs) firstGroups
        bestFirstSize = length . fst $ head validFirstGroups
        candidates    = takeWhile ((<=bestFirstSize) . length)
            $ map fst validFirstGroups
    in
        minimum $ map product candidates

answer2 :: Int
answer2 =
    let
-- optimistic setting, maybe there is no way to partition the
-- remaining presents in the 3 other spaces for a given first
-- group. This thing actually work so I let go :D
        groups     = map fst (selectTarget (sum weights `quot` 4) weights)
        bestSize   = length $ head groups
        candidates = takeWhile ((<=bestSize) . length) groups
    in  minimum $ map product candidates

select :: [Int] -> [(Int, [Int])]
select []     = []
select (x:xs) = (x, xs) : [ (y, x : ys) | (y, ys) <- select xs ]

selectTarget :: Int -> [Int] -> [([Int], [Int])]
selectTarget target xs = go target xs []
  where
    go target _  _ | target < 0 = []
    go 0      xs rest           = [([], xs ++ rest)]
    go _      [] _              = []
    go target l@(x:xs) rest =
        let takeNone = go target xs (x : rest)
            takeOne  = map (first ((:) x)) (go (target - x) xs rest)
        in  takeNone ++ takeOne

-- Fun with StateT to compute all possible partitions
-- firstGroups' xs = flip evalStateT xs $ do
--   let target = sum xs `quot` 4
--   g1 <- StateT (selectTarget target)
--   g2 <- StateT (selectTarget target)
--   guard . not $ null g2
--   g3 <- StateT (selectTarget target)
--   guard . not $ null g3
--   return g1

weights =
    [ 1
    , 2
    , 3
    , 5
    , 7
    , 13
    , 17
    , 19
    , 23
    , 29
    , 31
    , 37
    , 41
    , 43
    , 53
    , 59
    , 61
    , 67
    , 71
    , 73
    , 79
    , 83
    , 89
    , 97
    , 101
    , 103
    , 107
    , 109
    , 113
    ]

testWeights :: [Int]
testWeights = [1 .. 5] ++ [7 .. 11]
