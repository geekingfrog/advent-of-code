module Day17 (answer1, answer2) where

answer1 :: Int
answer1 = length $ arrangements target containers

answer2 :: Int
answer2 =
    let combinations = arrangements target containers
        minNumber    = minimum $ map length combinations
    in  length $ filter ((==) minNumber . length) combinations

containers =
    [11, 30, 47, 31, 32, 36, 3, 1, 5, 3, 32, 36, 15, 11, 46, 26, 28, 1, 19, 3]
target = 150

arrangements :: Int -> [Int] -> [[Int]]
arrangements target cs = filter (not . null) $ go target cs [[]]
  where
    go :: Int -> [Int] -> [[Int]] -> [[Int]]
    go target cs acc | target < 0  = [[]]
                     | target == 0 = acc
    go target [] acc = []
    go target (c:cs) acc =
        go (target - c) cs (map ((:) c) acc) ++ go target cs acc
