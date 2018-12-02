module Y2018.Day01 (answer1, answer2) where

import Data.Maybe
import Data.Foldable
import qualified Data.Set as Set

answer1, answer2 :: IO ()
answer1 = foldl' (+) 0 <$> getFreqs >>= print

answer2 = snd . loop (mempty, 0) . cycle <$> getFreqs >>= print
 where
  loop (!seen, !current) (x : xs) = if current `elem` seen
    then (seen, current)
    else loop (Set.insert current seen, current + x) xs
  loop _ [] = error "impossible"

getFreqs :: IO [Int]
getFreqs = map read . lines <$> readFile "data/2018/day01.txt"
