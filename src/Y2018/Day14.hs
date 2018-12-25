{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Y2018.Day14 (answer1, answer2) where


import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO
import Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Data.Foldable
import Data.Maybe
import Control.Monad
import Data.Functor

import qualified Control.Monad.Loops as Loops
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST
import Control.Monad.Primitive
import Data.STRef

import qualified Data.Vector.Unboxed as UV

answer1, answer2 :: IO ()
answer1 = print $ scoreM 633601
answer2 = print $ findTarget $ digits 633601

scoreM :: Int -> Int
scoreM n = runST $ do
  v <- MV.new (n+12)
  l <- newSTRef 2
  i <- newSTRef 0
  j <- newSTRef 1
  MV.write v 0 3
  MV.write v 1 7
  Loops.whileM_ ((< (n+12)) <$> readSTRef l) (makeRecipeM i j l v)
  foldM (\s k -> (+ (s*10)) <$> MV.read v k) 0 [n..n+9]

makeRecipeM sti stj stl v = do
    i <- readSTRef sti
    j <- readSTRef stj
    l <- readSTRef stl
    ri <- MV.read v i
    rj <- MV.read v j
    let ds = digits (ri + rj)

    let l' = l + length ds
    writeSTRef sti $ (i + ri + 1) `rem` l'
    writeSTRef stj $ (j + rj + 1) `rem` l'
    writeSTRef stl l'
    forM_ (zip [0..] ds) $ \(k,d) -> MV.write v (l+k) d



findTarget :: [Int] -> Int
findTarget target = runST $ do
  let n = length target
  stSeenUntil <- newSTRef 0
  v <- MV.new n
  sti <- newSTRef 0
  stj <- newSTRef 1
  stl <- newSTRef 2
  stv <- newSTRef v
  MV.write v 0 3
  MV.write v 1 7

  Loops.untilJust $ do
    seenUntil <- readSTRef stSeenUntil
    l <- readSTRef stl
    v <- readSTRef stv
    when (l >= MV.length v - 2) (MV.grow v l >>= writeSTRef stv)
    v <- readSTRef stv

    if seenUntil + n >= l
       then makeRecipeM sti stj stl v $> Nothing
       else findAt target seenUntil v >>= \case
          True -> pure (Just seenUntil)
          False -> modifySTRef stSeenUntil (+1) $> Nothing


findAt target i v =
  and <$> mapM (\(k, d) -> (== d) <$> MV.read v k) (zip [i..] target)


digits :: Int -> [Int]
digits 0 = [0]
digits i = go [] i
  where go acc 0 = acc
        go acc d =
            let (q, r) = quotRem d 10
            in go (r:acc) q


test :: IO ()
test = do
  print $ scoreM 5    == 0124515891
  print $ scoreM 9    == 5158916779
  print $ scoreM 18   == 9251071085
  print $ scoreM 2018 == 5941429882

  putStrLn ""
  print $ findTarget [5,1,5,8,9] == 9
  print $ findTarget [0,1,2,4,5] == 5
  print $ findTarget [5,9,4,1,4] == 2018

  print "done"
