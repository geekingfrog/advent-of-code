{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Y2017.Day14 (answer1, answer2) where

import Control.Monad
import GHC.Word
import Numeric
import Data.Bits
import Data.Foldable

import Data.Array ((!))
import qualified Data.Array as A

import qualified Data.Vector as V
import qualified Y2017.Day10 as H -- knot hash related stuff
import qualified Data.Tree as T
import qualified Data.Graph as G

answer1, answer2 :: Int
answer1 = countGridFree input
answer2 =
    let grid = toGrid input
        (_, (bx, by)) = A.bounds grid
        graph = toGraph grid
        rootNodes = fmap T.rootLabel (G.components graph)
        -- assume bx == by
        rootCoords = fmap (\n -> let (y,x) = n `quotRem` (by+1) in (x,y)) rootNodes
        usedCoords = filter (\c -> isUsed $ grid ! c) rootCoords
     in length usedCoords

data Status = Free | Used deriving Show

toGrid :: String -> A.Array (Int, Int) Status
toGrid key =
    let hashes = fmap (\i -> H.knotHash (key ++ "-" ++ show i)) [0..127]
        bits = fmap (concatMap hexToBits . toBytes) hashes
        statuses = (fmap . fmap) (\x -> if x then Used else Free) bits
        withIdxs = fmap (zip [0..]) statuses
        assocList = [((x, y), s) | (y, row) <- zip [0..] withIdxs, (x, s) <- row]
     in A.array ((0,0), (127,127)) assocList

toGraph :: A.Array (Int, Int) Status -> G.Graph
toGraph arr =
    let (_, (mx, my)) = A.bounds arr
    in G.buildG (0, (mx+1) * (my+1) - 1) (toEdges arr)


toEdges :: A.Array (Int, Int) Status -> [G.Edge]
toEdges arr = concatMap (getEdges arr) (A.indices arr)

getEdges :: A.Array (Int, Int) Status -> (Int, Int) -> [G.Edge]
getEdges arr (x,y) =
    let bounds = A.bounds arr
        (_, (bx,by)) = bounds
        ns = filter (inBounds bounds) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        a = y * (bx+1) + x
     in case arr ! (x,y) of
            Used -> do
                (nx, ny) <- ns
                guard $ isUsed (arr ! (nx, ny))
                let b = ny * (bx+1) + nx
                pure (a, b)
            Free -> []

inBounds :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inBounds ((minx, miny), (maxx, maxy)) (x, y) =
    minx <= x && x <= maxx && miny <= y && y <= maxy

isUsed Free = False
isUsed Used = True

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
