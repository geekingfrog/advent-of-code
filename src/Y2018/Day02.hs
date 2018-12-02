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

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

answer1 :: IO ()
answer1 = do
  ids <- Tx.lines <$> Tx.IO.readFile "./data/2018/day02.txt"
  let groups = map (Tx.foldl' count Map.empty) ids
  let has2   = filter (not . null . Map.filter (== 2)) groups
  let has3   = filter (not . null . Map.filter (== 3)) groups
  print $ length has2 * length has3

answer2 :: IO ()
answer2 = do
  ids <- Tx.lines <$> Tx.IO.readFile "./data/2018/day02.txt"
  let boxes = do
        x <- ids
        y <- ids
        guard $ x /= y
        let c = commonLetters x y
        guard $ Tx.length c == Tx.length x - 1
        pure c

  case boxes of
    (x:_) -> Tx.IO.putStrLn x
    _ -> error "invalid input?"

count :: Map.Map Char Int -> Char -> Map.Map Char Int
count m k = Map.insertWith (+) k 1 m

commonLetters :: Tx.Text -> Tx.Text -> Tx.Text
commonLetters x y = Tx.pack . map fst . filter (uncurry (==)) $ Tx.zip x y
