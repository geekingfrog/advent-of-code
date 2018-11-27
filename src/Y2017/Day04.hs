module Y2017.Day04 (answer1, answer2) where

import qualified Data.HashSet as Set
import Data.List (sort)

type PassPhrase = [String]


answer1, answer2 :: IO Int
answer1 = do
    passPhrases <- input
    let c = length $ filter valid passPhrases
    pure c
answer2 = do
    passPhrases <- input
    let c = length $ filter valid2 passPhrases
    pure c


valid, valid2 :: PassPhrase -> Bool
valid s = length (Set.fromList s) == length s
valid2 s = length (Set.fromList (map sort s)) == length s


input :: IO [PassPhrase]
input = fmap words . lines <$> readFile "data/2017/day04.txt"
