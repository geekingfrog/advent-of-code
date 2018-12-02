module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (die)
import Control.Monad
import Data.Monoid

import qualified Y2015.Day01 as D01
import qualified Y2015.Day02 as D02
import qualified Y2015.Day03 as D03
import qualified Y2015.Day04 as D04
import qualified Y2015.Day05 as D05
import qualified Y2015.Day06 as D06
import qualified Y2015.Day07 as D07
import qualified Y2015.Day08 as D08
import qualified Y2015.Day09 as D09
import qualified Y2015.Day10 as D10
import qualified Y2015.Day11 as D11
import qualified Y2015.Day12 as D12
import qualified Y2015.Day13 as D13
import qualified Y2015.Day14 as D14
import qualified Y2015.Day15 as D15
import qualified Y2015.Day16 as D16
import qualified Y2015.Day17 as D17
import qualified Y2015.Day18 as D18
import qualified Y2015.Day19 as D19
import qualified Y2015.Day20 as D20
import qualified Y2015.Day21 as D21
import qualified Y2015.Day22 as D22
import qualified Y2015.Day23 as D23
import qualified Y2015.Day24 as D24
import qualified Y2015.Day25 as D25

import qualified Y2016.Day11 as Y2016D11
import qualified Y2016.Day15 as Y2016D15
import qualified Y2016.Day19 as Y2016D19
import qualified Y2016.Day22 as Y2016D22


import qualified Y2017.Day01 as Y2017D01
import qualified Y2017.Day02 as Y2017D02
import qualified Y2017.Day03 as Y2017D03
import qualified Y2017.Day04 as Y2017D04
import qualified Y2017.Day05 as Y2017D05
import qualified Y2017.Day06 as Y2017D06
import qualified Y2017.Day07 as Y2017D07
import qualified Y2017.Day08 as Y2017D08
import qualified Y2017.Day09 as Y2017D09
import qualified Y2017.Day10 as Y2017D10
import qualified Y2017.Day11 as Y2017D11
import qualified Y2017.Day12 as Y2017D12
import qualified Y2017.Day13 as Y2017D13
import qualified Y2017.Day14 as Y2017D14
import qualified Y2017.Day15 as Y2017D15
import qualified Y2017.Day16 as Y2017D16
import qualified Y2017.Day17 as Y2017D17
import qualified Y2017.Day18 as Y2017D18
import qualified Y2017.Day19 as Y2017D19
import qualified Y2017.Day20 as Y2017D20
import qualified Y2017.Day21 as Y2017D21
import qualified Y2017.Day22 as Y2017D22
import qualified Y2017.Day23 as Y2017D23
import qualified Y2017.Day24 as Y2017D24
import qualified Y2017.Day25 as Y2017D25

import qualified Y2018.Day01 as Y2018D01
import qualified Y2018.Day02 as Y2018D02

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) $ do
        progName <- getProgName
        die
            (  "Usage: "
            <> progName
            <> " Y day problemNumber. Ex: "
            <> progName
            <> " 2015 7 1"
            )
    let (year:day:pbNumber:rest) = map read args :: [Int]
    case year of
        2015 -> run2015 day pbNumber
        2016 -> run2016 day pbNumber
        2017 -> run2017 day pbNumber
        2018 -> run2018 day pbNumber
        _    -> die $ "year unknown: " <> show year


run2015 :: Int -> Int -> IO ()
run2015 day pbNumber = case day * 10 + pbNumber of
    11  -> D01.answer1
    12  -> D01.answer2
    21  -> D02.answer1
    22  -> D02.answer2
    31  -> D03.answer1
    32  -> D03.answer2
    41  -> D04.answer1
    42  -> D04.answer2
    51  -> D05.answer1
    52  -> D05.answer2
    61  -> D06.answer1
    62  -> D06.answer2
    71  -> D07.answer1
    72  -> D07.answer2
    81  -> D08.answer1
    82  -> D08.answer2
    91  -> D09.answer1
    92  -> D09.answer2
    101 -> D10.answer1
    102 -> D10.answer2
    111 -> D11.answer1
    112 -> D11.answer2
    121 -> D12.answer1
    122 -> D12.answer2
    131 -> D13.answer1
    132 -> D13.answer2
    141 -> D14.answer1
    142 -> D14.answer2
    151 -> D15.answer1
    152 -> D15.answer2
    161 -> D16.answer1
    162 -> D16.answer2
    171 -> D17.answer1
    172 -> D17.answer2
    181 -> D18.answer1
    182 -> D18.answer2
    191 -> D19.answer1
    192 -> D19.answer2
    201 -> D20.answer1
    202 -> D20.answer2
    211 -> D21.answer1
    212 -> D21.answer2
    221 -> D22.answer1
    222 -> D22.answer2
    231 -> D23.answer1
    232 -> D23.answer2
    241 -> D24.answer1
    242 -> D24.answer2
    251 -> D25.answer1
    _   -> putStrLn "Invalid puzzle number"

run2016 :: Int -> Int -> IO ()
run2016 day pbNumber = case day * 10 + pbNumber of
    111 -> Y2016D11.answer1
    112 -> Y2016D11.answer2
    151 -> Y2016D15.answer1
    152 -> Y2016D15.answer2
    192 -> Y2016D19.answer2
    221 -> Y2016D22.answer1 >>= print
    222 -> Y2016D22.answer2 >>= print
    _   -> print "Invalid puzzle number"

run2017 :: Int -> Int -> IO ()
run2017 day pbNumber = case day * 10 + pbNumber of
    11 -> Y2017D01.answer1
    12 -> Y2017D01.answer2
    21 -> Y2017D02.answer1
    22 -> Y2017D02.answer2
    31 -> Y2017D03.answer1
    32 -> Y2017D03.answer2
    41 -> Y2017D04.answer1
    42 -> Y2017D04.answer2
    51 -> Y2017D05.answer1
    52 -> Y2017D05.answer2
    61 -> Y2017D06.answer1
    62 -> Y2017D06.answer2
    71 -> Y2017D07.answer1
    72 -> Y2017D07.answer2
    81 -> Y2017D08.answer1
    82 -> Y2017D08.answer2
    91 -> Y2017D09.answer1
    92 -> Y2017D09.answer2
    101 -> Y2017D10.answer1
    102 -> Y2017D10.answer2
    111 -> Y2017D11.answer1
    112 -> Y2017D11.answer2
    121 -> Y2017D12.answer1
    122 -> Y2017D12.answer2
    131 -> Y2017D13.answer1
    132 -> Y2017D13.answer2
    141 -> Y2017D14.answer1
    142 -> Y2017D14.answer2
    151 -> Y2017D15.answer1
    152 -> Y2017D15.answer2
    161 -> Y2017D16.answer1
    162 -> Y2017D16.answer2
    171 -> Y2017D17.answer1
    172 -> Y2017D17.answer2
    181 -> Y2017D18.answer1
    182 -> Y2017D18.answer2
    191 -> Y2017D19.answer1
    192 -> Y2017D19.answer2
    201 -> Y2017D20.answer1
    202 -> Y2017D20.answer2
    211 -> Y2017D21.answer1
    212 -> Y2017D21.answer2
    221 -> Y2017D22.answer1
    222 -> Y2017D22.answer2
    231 -> Y2017D23.answer1
    232 -> Y2017D23.answer2
    241 -> Y2017D24.answer1
    242 -> Y2017D24.answer2
    251 -> Y2017D25.answer1
    _   -> putStrLn "Invalid puzzle number"

run2018 :: Int -> Int -> IO ()
run2018 day pbNumber = case day * 10 + pbNumber of
    11 -> Y2018D01.answer1
    12 -> Y2018D01.answer2
    21 -> Y2018D02.answer1
    22 -> Y2018D02.answer2
    _ -> print "Invalid puzzle number"
