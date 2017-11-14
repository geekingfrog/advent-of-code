module Day10 (answer1, answer2) where

import Data.List (group)

answer1 :: Int
answer1 = length $ fullSequence !! 40

answer2 :: Int
answer2 = length $ fullSequence !! 50

fullSequence = iterate (expandList . groupList) input

groupList :: Eq a => [a] -> [(Int, a)]
groupList xs = map (\l -> (length l, head l)) (group xs)

expandList :: [(Int, Int)] -> [Int]
expandList xs = reverse $ go xs []
  where
    go []          acc = acc
    go ((a, b):xs) acc = go xs (b : a : acc)

input = [3, 1, 1, 3, 3, 2, 2, 1, 1, 3]
