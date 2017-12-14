module Y2017.Day14 (answer1, answer2) where

import Control.Monad
import GHC.Word
import Numeric
import Data.Bits
import Data.Foldable

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Y2017.Day10 as H -- knot hash related stuff
import qualified Y2017.Day12 as G -- graph related stuff

answer1, answer2 :: Int
answer1 = countGridFree input
answer2 = length $ G.findGroups $ toGraph input

toGraph :: String -> G.Graph
toGraph key =
    let makeRow s = V.fromList $ concatMap hexToBits $ toBytes $ H.knotHash s
        grid = V.fromList $ map (\s -> makeRow $ key ++ "-" ++ show s) [0..127]
        l = buildAdjacencyList grid
        graph = G.buildGraph l
     in graph

buildAdjacencyList :: V.Vector (V.Vector Bool) -> [(Int, [Int])]
buildAdjacencyList grid = do
    x <- [0..127]
    y <- filter odd [0..127]
    let i = y * 128 + x
    let ns =
            [ n
            | a <- [-1 .. 1]
            , b <- [-1 .. 1]
            , let ny = y - a
            , let nx = x - b
            , let n  = 128 * ny + nx
            , ny /= y
            , nx /= x
            , ny >= 0 && ny < 128 && nx >= 0 && nx < 128
            , (grid ! ny) ! nx
            ]
    pure (i, ns)

countGridFree key = sum $ map (\s -> countFree $ H.knotHash (key ++ "-" ++ show s)) [0..127]

countFree :: String -> Int
countFree h = length $ filter id $ concatMap hexToBits $ toBytes h

hexToBits :: Word8 -> [Bool]
hexToBits w = map (/= 0) [w .&. 8, w .&. 4, w .&. 2, w .&. 1]

toBytes :: String -> [Word8]
toBytes "" = []
toBytes (x:xs) = case readHex (x:"") of
                   [(n, "")] -> fromIntegral n : toBytes xs
                   _ -> error "invalid hex"

input = "jxqlasbh"
test = "flqrgnkx"
