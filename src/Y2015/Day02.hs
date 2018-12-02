module Y2015.Day02 (answer1, answer2) where

import Control.Monad (fmap)
import Data.List.Split (splitOn)
import Data.List (sort)

answer1 :: IO ()
answer1 = sum . map giftWrap <$> getData >>= print

answer2 :: IO ()
answer2 = sum . map giftRibbon <$> getData >>= print

getData = do
    raw <- fmap lines (readFile "./data/02.txt")
    let dims = [ sort $ map read (splitOn "x" l) | l <- raw ] :: [[Int]]
    return dims

giftWrap (l:w:h:_) = l * w + 2 * (l * w + w * h + h * l)
giftWrap _         = undefined -- keep -Wall happy

giftRibbon (l:w:h:_) = 2 * (l + w) + l * w * h
giftRibbon _         = undefined -- keep -Wall happy
