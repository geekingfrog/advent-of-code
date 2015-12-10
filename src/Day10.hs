module Day10 (answer1, answer2) where

answer1 :: Int
answer1 = length $ fullSequence !! 40

answer2 :: Int
answer2 = length $ fullSequence !! 50

fullSequence = iterate (expandList . groupList) input

groupList :: Eq a => [a] -> [(Int, a)]
groupList [] = []
groupList (x:xs) = go 1 x xs
  where go acc x [] = [(acc, x)]
        go acc x (y:ys)
          | x == y = go (acc+1) x ys
          | otherwise = (acc, x) : go 1 y ys

expandList :: [(Int, Int)] -> [Int]
expandList xs = reverse $ go xs []
  where go [] acc = acc
        go ((a,b):xs) acc = go xs (b : a : acc)

input = [3,1,1,3,3,2,2,1,1,3]
