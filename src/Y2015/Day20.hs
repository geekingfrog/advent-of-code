module Y2015.Day20 (answer1, answer2) where

import Data.List (nub, sort)
import Data.Numbers.Primes

answer1 :: Integer
answer1 =
    let presents = map numberOfPresents [1 ..]
        pairs    = zip [1 ..] presents
    in  fst . head $ dropWhile ((>=) targetPresent . snd) pairs

answer2 :: Integer
answer2 =
    let presents = map numberOfPresents' [1 ..]
        pairs    = zip [1 ..] presents
    in  fst . head $ dropWhile ((>=) targetPresent . snd) pairs

targetPresent = 36000000

numberOfPresents :: Integer -> Integer
numberOfPresents houseNumber = sum $ map (*10) $ divisors houseNumber

numberOfPresents' houseNumber =
    let limit       = houseNumber `quot` 50
        actualElves = dropWhile (<=limit) $ divisors houseNumber
    in  sum $ map (*11) actualElves

divisors = sort . combine . primeFactors

combine :: [Integer] -> [Integer]
combine []     = [1]
combine (x:xs) = let next = combine xs in nub $ x : map (*x) next ++ next
