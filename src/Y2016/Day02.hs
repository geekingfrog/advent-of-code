module Y2016.Day02 (answer1, answer2) where

import Control.Monad (liftM)
import Data.List.Split (splitOn)
import Data.List (sort)

answer1 :: IO Int
answer1 = do
    dimensions <- getData
    return $ sum (map giftWrap dimensions)


answer2 = do
    dimensions <- getData
    return $ sum (map giftRibbon dimensions)

getData = do
    raw <- liftM lines (readFile "./data/02.txt")
    let dims = [ sort $ map read (splitOn "x" l) | l <- raw ] :: [[Int]]
    return dims

giftWrap (l:w:h:_) = l * w + 2 * (l * w + w * h + h * l)
giftWrap _         = undefined -- keep -Wall happy

giftRibbon (l:w:h:_) = 2 * (l + w) + l * w * h
giftRibbon _         = undefined -- keep -Wall happy
