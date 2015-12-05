module Day05 (answer1, answer2) where

import Control.Monad (liftM)
import Control.Applicative (liftA2)
import Data.List (find, tails)
import Data.Maybe (mapMaybe)

answer1 :: IO Int
answer1 = do
  words <- getData
  let niceWords = filter (\s -> hasThreeVowels s && hasSameLetterTwiceInARow s && not (isNaughty s)) words
  return $ length niceWords

getData = liftM lines (readFile "./data/05.txt")

hasThreeVowels :: String -> Bool
hasThreeVowels s = length (filter isVowel s) >= 3
  where isVowel 'a' = True
        isVowel 'e' = True
        isVowel 'i' = True
        isVowel 'o' = True
        isVowel 'u' = True
        isVowel _   = False

hasSameLetterTwiceInARow :: String -> Bool
hasSameLetterTwiceInARow s = any isSame $ zip s (tail s)
  where isSame (a, b) = a == b

isNaughty s = any badCombo $ zip s (tail s)
  where badCombo ('a', 'b') = True
        badCombo ('c', 'd') = True
        badCombo ('p', 'q') = True
        badCombo ('x', 'y') = True
        badCombo _          = False

---- day 2
answer2 :: IO Int
answer2 = do
  words <- getData
  let niceWords = filter (\s -> pairWithNoOverlap s && hasIntercalated s) words
  return $ length niceWords

pairWithPos s = zip3 [0..] s (tail s)
samePair (_, a, b) (_, c, d) = a == c && b == d

pairWithNoOverlap s = (not . null) (filter (not . overlap) (allSamePairs s))

isSamePair (_, a, b) (_, c, d) = a == c && b == d

samePairs _ [] = []
samePairs p (x:xs) = if isSamePair p x then (p,x) : samePairs p xs else samePairs p xs

allSamePairs s = concatMap mapper $ tails $ pairWithPos s
  where mapper [] = []
        mapper (x:xs) = samePairs x xs

overlap ((i, _, _), (j, _, _)) = j == i+1

hasIntercalated s = let
  pairs = zip3 s (tail s) (drop 2 s)
  hasRepeat (a, _, c) = a == c
  in (not . null) $ filter hasRepeat pairs
