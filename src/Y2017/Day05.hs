{-# LANGUAGE RankNTypes #-}

module Y2017.Day05 (answer1, answer2) where

import Control.Monad.Primitive
import Control.Monad.ST.Strict
import qualified Data.Vector.Mutable as MV

answer1, answer2 :: IO ()
answer1 = do
    raw <- readFile "./data/2017/day05.txt"
    print $ countJumps $ map read $ lines raw
answer2 = do
    raw <- readFile "./data/2017/day05.txt"
    print $ countJumps2 $ map read $ lines raw


countJumps :: [Int] -> Int
countJumps jumps = runST $ do
    js <- initStack jumps
    loop js (const (+1)) 0 0

countJumps2 :: [Int] -> Int
countJumps2 jumps = runST $ do
    js <- initStack jumps
    let f v = if v >= 3 then subtract 1 else (+1)
    loop js f 0 0

initStack :: [a] -> (forall s . ST s (MV.MVector s a))
initStack jumps = do
    let n = length jumps
    js <- MV.new n
    mapM_ (\(v, i) -> MV.write js i v) (zip jumps [0 ..])
    pure js


loop
    :: (PrimMonad m)
    => MV.MVector (PrimState m) Int
    -> (Int -> Int -> Int)
    -> Int
    -> Int
    -> m Int
loop js f i k = if k >= MV.length js
    then pure i
    else do
        j <- MV.read js k
        MV.modify js (f j) k
        loop js f (i + 1) (k + j)

testJumps :: [Int]
testJumps = [0, 3, 0, 1, -3]
