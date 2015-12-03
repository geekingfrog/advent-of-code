module Day01 (answer1, answer2) where

import Data.List (foldl', scanl)

answer1 :: IO Int
answer1 = do
  parens <- getData
  let result = foldl' folder 0 parens
  return result

answer2 :: IO Int
answer2 = do
  parens <- getData
  let steps = zip [0..] (scanl folder 0 parens)
  let firstBasement = head $ dropWhile (\(_, acc) -> acc /= -1) steps
  return $ fst firstBasement

getData :: IO String
getData = readFile "./data/01_1.txt" >>= return . filter (\c -> c == '(' || c == ')')

folder acc '(' = acc + 1
folder acc ')' = acc - 1
