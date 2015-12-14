module Day14 (answer1, answer2) where

import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)

answer1 :: Int
answer1 = maximum $ map (distanceRun inputTime) reindeers

answer2 :: Int
answer2 = let
  winners = concatMap winnersAtRound [1..inputTime]
  points = map length $ group $ sort winners
  in maximum points

data Reindeer = Reindeer {
  name :: String,
  dist :: Int,
  trun :: Int,
  trest :: Int
} deriving (Show, Eq, Ord)

inputTime = 2503

reindeers = [
  Reindeer "Rudolph" 22   8  165,
  Reindeer "Cupid"   8    17 114,
  Reindeer "Prancer" 18   6  103,
  Reindeer "Donner"  25   6  145,
  Reindeer "Dasher"  11   12 125,
  Reindeer "Comet"   21   6  121,
  Reindeer "Blitzen" 18   3  50,
  Reindeer "Vixen"   20   4  75,
  Reindeer "Dancer"  7    20 119
  ]

distanceRun time (Reindeer _ d run rest) = let
  x = run * (time `quot` (run + rest))
  y = min run (time `mod` (run + rest))
  in d * (x + y)

winnersAtRound :: Int -> [Reindeer]
winnersAtRound t = let
  distances = map (distanceRun t) reindeers
  maxDist = maximum distances
  pairs = zip distances reindeers
  winners = filter (\(d, _) -> d == maxDist) pairs
  in map snd winners
