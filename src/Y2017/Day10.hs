{-# LANGUAGE BangPatterns #-}

module Y2017.Day10 (answer1, answer2, knotHash, showHex, toBytes) where

import GHC.Word
import qualified Numeric
import Data.List.Split (chunksOf)
import Data.Bits
import Data.Char
import qualified Data.Vector as V
import Data.Monoid

answer1 :: IO ()
answer1 =
    let (_, _, v) = foldl hashStep (0, 0, V.fromList [0 .. 255]) input
    in  print $ (v V.! 0) * (v V.! 1)

answer2 :: IO ()
answer2 = putStrLn $ knotHash input2

hashStep :: (Int, Int, V.Vector Int) -> Int -> (Int, Int, V.Vector Int)
hashStep (!skip, pos, !v) l =
    let n        = V.length v
        (v1, v2) = V.splitAt pos v
        v'       = V.reverse $ V.take l $ v2 <> v1
        us       = [ ((i + pos) `mod` n, v' V.! i) | i <- [0 .. l - 1] ]
    in  (skip + 1, (pos + skip + l) `mod` n, v V.// us)

hash :: V.Vector Int -> [Int] -> V.Vector Int
hash v l = third $ foldl round (0, 0, v) [0 .. 63]
  where
    round r _ = foldl hashStep r l
    third (_, _, x) = x

densify :: [Word8] -> [Word8]
densify l = foldl1 xor <$> chunksOf 16 l

knotHash :: String -> String
knotHash s =
    let bytesIn  = toBytes s ++ [17, 31, 73, 47, 23]
        bytesOut = map fromIntegral $ V.toList $ hash
            (V.fromList [0 .. 255])
            (map fromIntegral bytesIn)
        dense = densify bytesOut
    in  concatMap showHex dense

showHex :: (Integral a, Show a) => a -> String
showHex w = case Numeric.showHex w "" of
    [x] -> '0' : [x]
    x   -> x

toBytes :: String -> [Word8]
toBytes = map (fromIntegral . ord)

input :: [Int]
input = [199, 0, 255, 136, 174, 254, 227, 16, 51, 85, 1, 2, 22, 17, 7, 192]

input2 :: String
input2 = "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192"

test :: [Int]
test = [3, 4, 1, 5]
