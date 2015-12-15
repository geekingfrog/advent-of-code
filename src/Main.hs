module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07
import qualified Day08 as D08
import qualified Day09 as D09
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15

main :: IO ()
main = do
  (day : pbNumber : rest) <- liftM (map read) getArgs :: IO [Int]
  case day * 10 + pbNumber of
    11 -> D01.answer1 >>= print
    12 -> D01.answer2 >>= print
    21 -> D02.answer1 >>= print
    22 -> D02.answer2 >>= print
    31 -> D03.answer1 >>= print
    32 -> D03.answer2 >>= print
    41 -> putStrLn D04.answer1
    42 -> putStrLn D04.answer2
    51 -> D05.answer1 >>= print
    52 -> D05.answer2 >>= print
    61 -> D06.answer1 >>= print
    62 -> D06.answer2 >>= print
    71 -> D07.answer1 >>= print
    72 -> D07.answer2 >>= print
    81 -> D08.answer1 >>= print
    82 -> D08.answer2 >>= print
    91 -> D09.answer1 >>= print
    92 -> D09.answer2 >>= print
    101 -> print D10.answer1
    102 -> print D10.answer2
    111 -> putStrLn D11.answer1
    112 -> putStrLn D11.answer2
    121 -> D12.answer1 >>= print
    122 -> D12.answer2 >>= print
    131 -> D13.answer1 >>= print
    132 -> D13.answer2 >>= print
    141 -> print D14.answer1
    142 -> print D14.answer2
    151 -> print D15.answer1
    152 -> print D15.answer2
    _ -> print "not done yet"
