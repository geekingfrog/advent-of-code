module Day15 (answer1, answer2) where

-- bruteforce solution, not totally happy about that though...

import Data.List (foldl', maximumBy)
import Data.Ord

answer1 :: Int
answer1 = maximum $ map mixCost (allMixes target)

answer2 :: Int
answer2 = maximum $ map mixCost (allMixesWithCalories target targetCalories)

data Ingredient = Ingredient {
  name       :: String,
  capacity   :: Int,
  durability :: Int,
  flavor     :: Int,
  texture    :: Int,
  calorie    :: Int
} deriving (Show, Eq)

target = 100
targetCalories = 500

sugar = Ingredient "Sugar" 3 0 0 (-3) 2
sprinkles = Ingredient "Sprinkles" (-3) 3 0 0 9
candy = Ingredient "Candy" (-1) 0 4 0 1
chocolate = Ingredient "Chocolate" 0 0 (-2) 2 8

allMixes n =
    [ mix
    | a <- [0 .. n]
    , b <- [0 .. n - a]
    , b >= 0
    , c <- [0 .. n - a - b]
    , c >= 0
    , let d = n - a - b - c
    , d >= 0
    , let mix = [(a, sugar), (b, sprinkles), (c, candy), (d, chocolate)]
    ]

allMixesWithCalories n cal =
    [ mix
    | a <- [0 .. n]
    , b <- [0 .. n - a]
    , b >= 0
    , c <- [0 .. n - a - b]
    , c >= 0
    , let d = n - a - b - c
    , d >= 0
    , let mix = [(a, sugar), (b, sprinkles), (c, candy), (d, chocolate)]
    , calories mix == cal
    ]

mixCost :: [(Int, Ingredient)] -> Int
mixCost = prod . foldl' addIngredient initialState
  where
    initialState = (0, 0, 0, 0)
    addIngredient (capa, dura, flav, text) (n, i) =
        ( capa + n * capacity i
        , dura + n * durability i
        , flav + n * flavor i
        , text + n * texture i
        )
    sign a b c d = if any (<0) [a, b, c, d] then 0 else 1
    prod (a, b, c, d) = a * b * c * d * sign a b c d


calories :: [(Int, Ingredient)] -> Int
calories = foldl' (\acc (n, i) -> acc + n * calorie i) 0
