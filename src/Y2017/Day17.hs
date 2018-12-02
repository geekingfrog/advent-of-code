{-# LANGUAGE BangPatterns #-}

module Y2017.Day17 (answer1, answer2) where

import qualified Data.Vector as V
import Data.Maybe
import Data.Foldable
import Data.Monoid

answer1, answer2 :: IO ()
answer1 = print $ solve1 input
answer2 =
    let (_, _, val) = foldl' (step2 input) (0, 1, Nothing) [1 .. 50 * 10 ^ 6]
    in  print $ fromMaybe (error "nope") val

input :: Int
input = 367


solve1 stepSize =
    let (finalX, finalV) =
            foldl' (step stepSize) (0, V.singleton 0) [1 .. 2017]
    in  finalV V.! ((finalX + 1) `mod` V.length finalV)

step stepSize (!x, !v) !i =
    let x'                = (x + stepSize) `mod` V.length v + 1
        (vbefore, vafter) = V.splitAt x' v
        v'                = vbefore <> V.singleton i <> vafter
    in  (x', v')


step2 stepSize (!x, !n, val) !i =
    let x' = (x + stepSize) `mod` n
    in  if x' == 0 then (1, n + 1, Just i) else (x' + 1, n + 1, val)
