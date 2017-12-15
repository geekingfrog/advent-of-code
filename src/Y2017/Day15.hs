module Y2017.Day15 (answer1, answer2) where

import GHC.Word
import Data.Bits

answer1, answer2 :: Int
answer1 =
    let valsA = take (40 * 10 ^ 6) genA
        valsB = take (40 * 10 ^ 6) genB
    in  length $ filter (\(a, b) -> (a .&. 0xffff) == (b .&. 0xffff)) $ zip
            valsA
            valsB
answer2 =
    let valsA = take (5 * 10 ^ 6) $ filter ((==0) . (`mod`4)) genA
        valsB = take (5 * 10 ^ 6) $ filter ((==0) . (`mod`8)) genB
    in  length $ filter (\(a, b) -> (a .&. 0xffff) == (b .&. 0xffff)) $ zip
            valsA
            valsB

genA = tail $ iterate (genVal 16807) startA
genB = tail $ iterate (genVal 48271) startB

genVal f v = (v * f) `rem` 2147483647

startA, startB, testA, testB :: Word64
startA = 512
startB = 191
testA = 65
testB = 8921
