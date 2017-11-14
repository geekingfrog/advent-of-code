module Y2015.Day25 (answer1) where

answer1 :: Int
answer1 = code 3010 3019

code i j = go i j 20151125
  where
    go 1 1 acc = acc
    go i j acc =
        let (i', j') = if j == 1 then (1, i - 1) else (i + 1, j - 1)
        in  go i' j' ((acc * 252533) `mod` 33554393)
