module Y2015.Day10 (answer1, answer2) where

import Data.List (group)

answer1 :: IO ()
answer1 = print $ length $ fullSequence !! 40

answer2 :: IO ()
answer2 = print $ length $ fullSequence !! 50

fullSequence = iterate (expandList . groupList) input

groupList :: Eq a => [a] -> [(Int, a)]
groupList xs = map (\l -> (length l, head l)) (group xs)

expandList :: [(Int, Int)] -> [Int]
expandList xs = reverse $ go xs []
  where
    go []          acc = acc
    go ((a, b):xs) acc = go xs (b : a : acc)

input = [3, 1, 1, 3, 3, 2, 2, 1, 1, 3]
