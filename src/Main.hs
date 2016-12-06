module Main where

import System.Environment (getArgs)
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
import qualified Day16 as D16
import qualified Day17 as D17
import qualified Day18 as D18
import qualified Day19 as D19
import qualified Day20 as D20
import qualified Day21 as D21
import qualified Day22 as D22
import qualified Day23 as D23
import qualified Day24 as D24
import qualified Day25 as D25

main :: IO ()
main = do
  (day : pbNumber : rest) <- fmap (map read) getArgs :: IO [Int]
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
    161 -> D16.answer1 >>= print
    162 -> D16.answer2 >>= print
    171 -> print D17.answer1
    172 -> print D17.answer2
    181 -> D18.answer1 >>= print
    182 -> D18.answer2 >>= print
    191 -> print D19.answer1
    192 -> print D19.answer2
    201 -> print D20.answer1
    202 -> print D20.answer2
    211 -> print D21.answer1
    212 -> print D21.answer2
    221 -> print D22.answer1
    222 -> print D22.answer2
    231 -> D23.answer1 >>= print
    232 -> D23.answer2 >>= print
    241 -> print D24.answer1
    242 -> print D24.answer2
    251 -> print D25.answer1
    _ -> print "Invalid puzzle number"
