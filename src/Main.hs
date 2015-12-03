module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import qualified Day01 as D01
import qualified Day02 as D02

main :: IO ()
main = do
  (day : pbNumber : rest) <- liftM (map read) getArgs :: IO [Int]
  case day * 10 + pbNumber of
    11 -> D01.answer1 >>= print
    12 -> D01.answer2 >>= print
    21 -> D02.answer1 >>= print
    22 -> D02.answer2 >>= print
    _ -> print "not done yet"
