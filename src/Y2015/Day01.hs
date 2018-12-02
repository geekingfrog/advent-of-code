module Y2015.Day01 (answer1, answer2) where

import Data.List (foldl', scanl)

answer1 :: IO ()
answer1 = do
    parens <- getData
    let result = foldl' folder 0 parens
    print result

answer2 :: IO ()
answer2 = do
    parens <- getData
    let steps         = zip [0 ..] (scanl folder 0 parens)
    let firstBasement = head $ dropWhile (\(_, acc) -> acc /= -1) steps
    print $ fst firstBasement

getData :: IO String
getData =
    filter (\c -> c == '(' || c == ')') <$> readFile "./data/01.txt"

folder acc '(' = acc + 1
folder acc ')' = acc - 1
folder _ c = error $ "invalid symbol: " ++ [c]
