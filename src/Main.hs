module Main where

import System.Environment (getArgs)
import Control.Monad (liftM)
import qualified Day01 as D01

main :: IO ()
main = do
  (day : pbNumber : rest) <- getArgs >>= return . map read :: IO ([Int])
  case day * 10 + pbNumber of
    11 -> D01.answer1 >>= print
    12 -> D01.answer2 >>= print
    _ -> print "not done yet"
