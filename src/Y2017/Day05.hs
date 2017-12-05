module Y2017.Day05 (answer1, answer2) where

import Control.Monad.ST.Strict
import qualified Data.Vector.Mutable as MV

answer1, answer2 :: IO Int
answer1 = do
    raw <- readFile "./data/2017/day05.txt"
    pure $ countJumps $ map read $ lines raw
answer2 = do
    raw <- readFile "./data/2017/day05.txt"
    pure $ countJumps2 $ map read $ lines raw


countJumps :: [Int] -> Int
countJumps jumps = runST $ do
    let n = length jumps
    js <- MV.new n
    mapM_ (\(v, i) -> MV.write js i v) (zip jumps [0 ..])
    loop js 0 0

loop js i k = if k >= MV.length js
    then pure i
    else do
        j <- MV.read js k
        MV.modify js (+1)    k
        loop      js (i + 1) (k + j)


countJumps2 :: [Int] -> Int
countJumps2 jumps = runST $ do
    let n = length jumps
    js <- MV.new n
    mapM_ (\(v, i) -> MV.write js i v) (zip jumps [0 ..])
    loop2 js 0 0

loop2 js i k = if k >= MV.length js
    then pure i
    else do
        j <- MV.read js k
        let d = if j >= 3 then subtract 1 else (+1)
        MV.modify js d       k
        loop2     js (i + 1) (k + j)

testJumps :: [Int]
testJumps = [0, 3, 0, 1, -3]
