module Y2017.Day13 (answer1, answer2) where

import Data.Foldable


answer1, answer2 :: IO ()
answer1 = print $ foldl' (\acc (a,b) -> acc + a * b) 0 $ filter (caught 0) input
answer2 = print $ head [d | d <- [0..], not (any (caught d) input)]


caught :: Int -> (Int, Int) -> Bool
caught delay (l, d) = ((l+delay) `mod` (2 * (d-1))) == 0


test :: [(Int, Int)]
test = [(0,3),(1,2),(4,4),(6,4)]

input :: [(Int, Int)]
input =
    [ (0  , 4)
    , (1  , 2)
    , (2  , 3)
    , (4  , 4)
    , (6  , 8)
    , (8  , 5)
    , (10 , 6)
    , (12 , 6)
    , (14 , 10)
    , (16 , 8)
    , (18 , 6)
    , (20 , 9)
    , (22 , 8)
    , (24 , 6)
    , (26 , 8)
    , (28 , 8)
    , (30 , 12)
    , (32 , 12)
    , (34 , 12)
    , (36 , 12)
    , (38 , 10)
    , (40 , 12)
    , (42 , 12)
    , (44 , 14)
    , (46 , 8)
    , (48 , 14)
    , (50 , 12)
    , (52 , 14)
    , (54 , 14)
    , (58 , 14)
    , (60 , 12)
    , (62 , 14)
    , (64 , 14)
    , (66 , 12)
    , (68 , 12)
    , (72 , 14)
    , (74 , 18)
    , (76 , 17)
    , (86 , 14)
    , (88 , 20)
    , (92 , 14)
    , (94 , 14)
    , (96 , 18)
    , (98 , 18)
    ]
