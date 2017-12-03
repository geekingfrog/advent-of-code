module Y2017.Day03 (answer1, answer2) where

import Control.Monad.State.Strict

answer1, answer2 :: Int
answer1 = dist $ coord input
answer2 = error "wip 2"


coord :: Int -> (Int, Int)
coord n =
    let k = findClosestOddSqrt n
        k' = k `div` 2
        a = n - k * k
        (q, r) = a `quotRem` (k+1)
        (x, y) | a == 0 = (k', -k')
               | q == 0 = (k'+1, r - k' - 1)
               | q == 1 = (-r + k' + 1, k'+1)
               | q == 2 = (-k'-1, - r + k' + 1)
               | q == 3 = (r - k' - 1, -k' - 1)
               | otherwise = error "non exhaustive case"

    in (x, y)

dist (a, b) = abs a + abs b


-- find the closest odd number 2k+1 such that (2k+1)^2 < n <= (2k+3)^2
findClosestOddSqrt :: Int -> Int
findClosestOddSqrt n =
    let k = truncate (sqrt $ fromIntegral n) in if odd k then k else k-1



input = 265149
