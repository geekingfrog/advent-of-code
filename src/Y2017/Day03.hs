{-# LANGUAGE BangPatterns #-}

module Y2017.Day03 (answer1, answer2) where


import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as Map
import Control.Monad.Loops

answer1, answer2 :: Int
answer1 = dist $ coord input
answer2 = evalState
    (iterateUntil (>= input) walk)
    (1, Map.singleton (0, 0) 1)



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


walk :: State (Int, Map.HashMap (Int, Int) Int) Int
walk = do
    (n, vs) <- get
    let (x, y) = coord n
    let neighbors = [(x-a, y-b) | a <- [-1,0,1], b <- [-1,0,1]]
    let v = sum $ map (\k -> Map.lookupDefault 0 k vs) neighbors
    let n' = n + 1
    let !vs' = Map.insert (x,y) v vs
    put (n', vs')
    pure v


input = 265149
