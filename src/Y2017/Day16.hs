{-# LANGUAGE BangPatterns #-}

module Y2017.Day16 (answer1, answer2) where

import Data.Functor
import Data.Maybe
import Data.Foldable
import qualified Data.List.Split as L
import qualified Data.Vector as V

import Data.Hashable
import qualified Data.HashSet as Set

answer1, answer2 :: IO ()
answer1 = do
    moves <- parseInput
    putStrLn $ V.toList $ perform moves $ V.fromList ['a'..'p']
answer2 = do
    moves <- parseInput
    let start = V.fromList ['a'..'p']
        end = perform moves start
        states = iterate (perform moves) start
        period = findPeriod $ fmap V.toList states
        final = states !! ((10 ^ 9) `mod` period)
    putStrLn $ V.toList final

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving (Show)

perform :: [Move] -> V.Vector Char -> V.Vector Char
perform moves starting = foldl' move starting moves

findPeriod :: (Hashable a, Eq a) => [a] -> Int
findPeriod = go Set.empty
  where
    go !s (x:xs) | Set.member x s = length s
                | otherwise      = go (Set.insert x s) xs
    go _ _ = error "impossible"


move :: V.Vector Char -> Move -> V.Vector Char
move v (Spin s) =
    let n = V.length v
        ixs = V.fromList [ k | i <- [0 .. n-1], let k = (i - s) `mod` n ]
    in  V.backpermute v ixs
move v (Exchange a b) = v V.// [(b, v V.! a), (a, v V.! b)]
move v (Partner  a b) = case (V.findIndex (==a) v, V.findIndex (==b) v) of
    (Just ai, Just bi) -> move v (Exchange ai bi)
    _ -> error "cannot find dancer"

parseInput :: IO [Move]
parseInput = do
    raw <- readFile "./data/2017/day16.txt"
    pure $ parseMove <$> L.splitOn "," raw



parseMove (x:xs)
    | x == 's' = Spin (read xs)
    | x == 'x' = let [a, b] = L.splitOn "/" xs in Exchange (read a) (read b)
    | x == 'p' = let [a, '/', b] = xs in Partner a b
    | otherwise = error $ "cannot parse" ++ (x:xs)
parseMove [] = error "cannot parse"
