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

import qualified Y2017.Day01 as Y2017D01
import qualified Y2017.Day02 as Y2017D02
import qualified Y2017.Day03 as Y2017D03
import qualified Y2017.Day04 as Y2017D04

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
        _    -> die $ "year unknown: " <> show year


run2015 :: Int -> Int -> IO ()
run2015 day pbNumber = case day * 10 + pbNumber of
    11  -> D01.answer1 >>= print
    12  -> D01.answer2 >>= print
    21  -> D02.answer1 >>= print
    22  -> D02.answer2 >>= print
    31  -> D03.answer1 >>= print
    32  -> D03.answer2 >>= print
    41  -> putStrLn D04.answer1
    42  -> putStrLn D04.answer2
    51  -> D05.answer1 >>= print
    52  -> D05.answer2 >>= print
    61  -> D06.answer1 >>= print
    62  -> D06.answer2 >>= print
    71  -> D07.answer1 >>= print
    72  -> D07.answer2 >>= print
    81  -> D08.answer1 >>= print
    82  -> D08.answer2 >>= print
    91  -> D09.answer1 >>= print
    92  -> D09.answer2 >>= print
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
    _   -> print "Invalid puzzle number"

run2016 :: Int -> Int -> IO ()
run2016 day pbNumber = case day * 10 + pbNumber of
    111 -> print Y2016D11.answer1
    112 -> print Y2016D11.answer2
    151 -> print Y2016D15.answer1
    152 -> print Y2016D15.answer2
    192 -> print Y2016D19.answer2
    _   -> print "Invalid puzzle number"

run2017 :: Int -> Int -> IO ()
run2017 day pbNumber = case day * 10 + pbNumber of
    11 -> print Y2017D01.answer1
    12 -> print Y2017D01.answer2
    21 -> print Y2017D02.answer1
    22 -> print Y2017D02.answer2
    31 -> print Y2017D03.answer1
    32 -> print Y2017D03.answer2
    41 -> Y2017D03.answer1 >>= print
    42 -> Y2017D03.answer2 >>= print
    _   -> print "Invalid puzzle number"
