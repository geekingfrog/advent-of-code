{-# LANGUAGE OverloadedStrings #-}

module Y2018.Day09 (answer1, answer2) where

import Data.Maybe
import Control.Monad.Loops
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO
import           Data.Foldable
import qualified Data.Vector                   as V

import Data.List

answer1, answer2 :: IO ()
answer1 = print $ solve 459 71790
answer2 = print $ solve 459 (71790 * 100)

solve :: Int -> Int -> Int
solve n finalMarble =
  let r0 = Ring [] 0 []
      s0 = (r0, V.replicate n 0, 0)
      states = scanl gameStep s0 [1..finalMarble]
      (_, finalPoints, _) = last states
      finalScore = V.maximum finalPoints
  in finalScore

data Ring a = Ring [a] a [a]
type GameState = (Ring Int, V.Vector Int, Int)

gameStep :: GameState -> Int -> GameState
gameStep (r, points, idx) x =
  if x `mod` 23 == 0
    then let (p, r') = step23 r
             p' = (V.//) points [(idx, (p+x) + points V.! idx)]
          in (r', p', idx')
    else (step x r, points, idx')
  where idx' = (idx+1) `mod` V.length points

step :: Int -> Ring Int -> Ring Int
step x r = addMarble x (moveRight1 r)

step23 :: Ring Int -> (Int, Ring Int)
step23 r = removeMarble (iterate moveLeft1 r !! 7)

moveRight1, moveLeft1 :: Ring a -> Ring a
moveRight1 ring@(Ring l c r) = case r of
  []     -> case l of
              [] -> ring
              _ -> let l' = reverse (c:l)
                    in Ring [] (head l') (tail l')
  (x:xs) -> Ring (c:l) x xs

moveLeft1 ring@(Ring l c r) = case l of
  [] -> case r of
          [] -> ring
          _ -> let r' = reverse (c:r)
                in Ring (tail r') (head r') []
  (x:xs) -> Ring xs x (c:r)

addMarble :: a -> Ring a -> Ring a
addMarble x (Ring l c r) = Ring (c:l) x r

removeMarble :: Ring a -> (a, Ring a)
removeMarble (Ring l c r) = case (l, r) of
  ([], []) -> error "cannot remove marble from empty ring"
  (_, x:xs) -> (c, Ring l x xs)
  (x:xs, _) -> (c, Ring xs x r)

ringToText :: Show a => Ring a -> Tx.Text
ringToText (Ring l c r)
  =  Tx.unwords (fmap (Tx.pack . show) (reverse l))
  <> " (" <> Tx.pack (show c) <> ") "
  <> Tx.unwords (fmap (Tx.pack . show) r)

test :: IO ()
test = do
  let r0 = Ring [] 0 []
  let s0 = (r0, V.replicate 9 0, 0)
  let states = scanl gameStep s0 [1..27]
  mapM_ (\(r, _, _) -> Tx.IO.putStrLn $ ringToText r) states
  let (_, points, _) = states !! 27
  print points
