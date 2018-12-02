module Y2018.Day02
  ( answer1
  , answer2
  )
where

import           Control.Monad
import           Data.Maybe
import           Data.Foldable
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as Map

answer1 :: IO ()
answer1 = do
  ids <- lines <$> readFile "./data/2018/day02.txt"
  let groups = map (foldl' count Map.empty) ids
  let has2   = filter (not . null . Map.filter (== 2)) groups
  let has3   = filter (not . null . Map.filter (== 3)) groups
  print $ length has2 * length has3

answer2 :: IO ()
answer2 = do
  ids <- lines <$> readFile "./data/2018/day02.txt"
  let boxes = do
        x <- ids
        y <- ids
        guard $ x /= y
        let c = commonLetters x y
        guard $ length c == length x - 1
        pure c

  case boxes of
    (x:_) -> putStrLn x
    _ -> error "invalid input?"

count :: Map.Map Char Int -> Char -> Map.Map Char Int
count m k = Map.insertWith (+) k 1 m

commonLetters :: String -> String -> String
commonLetters x y = map fst . filter (uncurry (==)) $ zip x y
