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
    _ -> print "not done yet"
