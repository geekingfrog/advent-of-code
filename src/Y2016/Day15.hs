module Y2016.Day15 (answer1, answer2) where

import Control.Monad

type Disc = (Int, Int)

answer1, answer2 :: Int
answer1 = solve discs
answer2 = solve discs'
    where discs' = discs ++ [(7, 11)]

discs :: [Disc]
discs = [ (3, 5)
        , (9, 13)
        , (13, 17)
        , (0, 3)
        , (14, 19)
        , (6, 7)]

validDisc :: Int -> Disc -> Bool
validDisc k (a, b) = (k + a) `mod` b == 0

solve :: [Disc] -> Int
solve ds = head [k | k <- [1..], all (validDisc k) ds]
