module Day25 (answer1) where

answer1 :: Int
answer1 = code 3010 3019

code 1 1 = 20151125
code i j = let
  prev = if j == 1 then code 1 (i-1) else code (i+1) (j-1)
  in (252533 * prev) `mod` 33554393
