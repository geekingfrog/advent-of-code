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
import qualified Data.Array.ST as STA
import Control.Monad.ST as ST
import Data.STRef as ST
import Data.List as L

-- import Data.Vector ((!))
-- import qualified Data.Vector as V
import qualified Y2017.Day10 as H -- knot hash related stuff
-- import qualified Y2017.Day12 as G -- graph related stuff
import qualified Data.Graph as G

answer1, answer2 :: Int
answer1 = countGridFree input
answer2 = length $ G.components $ toGraph $ toGrid input
-- 9427 too high

data Status = Free | Used deriving Show

toGrid :: String -> A.Array (Int, Int) Status
toGrid key =
    let hashes = fmap (\i -> H.knotHash (key ++ "-" ++ show i)) [0..127]
        bits = fmap (concatMap hexToBits . toBytes) hashes
        statuses = (fmap . fmap) (\x -> if x then Used else Free) bits
        withIdxs = fmap (zip [0..]) statuses
    in error "wip"
    --     bits = concatMap (concat . L.transpose . map hexToBits . toBytes) hashes
    --     statuses = fmap (\x -> if x then Used else Free) bits
    -- in A.listArray ((0,0), (127, 127)) statuses

toGraph :: A.Array (Int, Int) Status -> G.Graph
toGraph arr =
    let (_, (mx, my)) = A.bounds arr
    in G.buildG (0, (mx+1) * (my+1) - 1) (toEdges arr)


toEdges :: A.Array (Int, Int) Status -> [G.Edge]
toEdges arr = concatMap (getEdges arr) (A.indices arr)
    -- let ((minx,miny), (maxx,maxy)) = A.bounds arr
    --     ixs = [(x,y) | y <- [miny, maxy], x <- [minx, maxx]]
    -- in concatMap (getEdges arr) ixs

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

-- -- toArray :: (Monad m, MA.MArray a Int m) => String -> m (a (Int, Int) Int)
-- toArray :: (STA.MArray a Int m) => String -> m (a (Int, Int) Int)
-- toArray key = do
--     -- let makeRow s = V.fromList $ concatMap hexToBits $ toBytes $ H.knotHash s
--     let h = concatMap hexToBits $ toBytes $ H.knotHash $ key ++ "-" ++ "0"
--     a <- MA.newArray ((1,1), (128,128)) (-1 :: Int)
--     forM_ [1..128] $ \y -> do
--         let row = concatMap hexToBits $ toBytes $ H.knotHash $ key ++ "-" ++ show (y-1)
--         forM_ (zip row [1..]) $ \(b, x) -> unless b (MA.writeArray a (x, y) 0)
--     pure a

-- -- grid where free bits are (-1) and used ones are 0
-- toArray :: String -> forall s. ST s (MV.MVector s (MV.MVector s Int))
-- toArray key = do
--     grid <- MV.replicateM 128 (MV.replicate 128 (-1))
--     -- let h = concatMap hexToBits $ toBytes $ H.knotHash $ key ++ "-" ++ "0"
--     forM_ [1..128] $ \y -> do
--         let row = concatMap hexToBits $ toBytes $ H.knotHash $ key ++ "-" ++ show (y-1)
--         forM_ (zip row [1..]) $ \(b, x) -> unless b $ do -- (MA.writeArray a (x, y) 0)
--             row <- MV.read grid y
--             MV.write row x 0
--     pure grid

-- countRegions :: String -> Int
-- countRegions key = runST $ do
--     r <- newSTRef 1
--     -- let _ = STA.runSTArray $ toArray key
--     let _ = STA.runSTArray $ do
--             grid <- toArray key
--             let coords = [(x,y) | x <- [1..128], y <- [1..128]]
--             forM_ coords $ \(x, y) -> do
--                 square <- MA.readArray grid (x,y)
--                 when (square == 0) $ do
--                     let region = 1
--                     -- region <- readSTRef r
--                     fillRegion grid region (x,y)
--                     -- writeSTRef r (region+1)
--             pure grid
--     readSTRef r

-- fillRegion grid regionNum coord = go grid regionNum [coord]
--   where
--       go _ _ [] = pure ()
--       go grid regionNum ((x,y):xs) = do
--           square <- get grid (x,y)
--           when (square == 0) $ do
--               set grid (x, y) regionNum
--               let neighbors = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
--               newNeighbors <- filterM (fmap (== 0) . get grid) neighbors
--               go grid regionNum (newNeighbors ++ xs)
--
-- get arr (x, y) = do
--     row <- MV.read arr y
--     MV.read row x
--
-- set arr (x, y) v = do
--     row <- MV.read arr y
--     MV.write row x v

-- toGraph :: String -> G.Graph
-- toGraph key =
--     let makeRow s = V.fromList $ concatMap hexToBits $ toBytes $ H.knotHash s
--         grid = V.fromList $ map (\s -> makeRow $ key ++ "-" ++ show s) [0..127]
--         l = buildAdjacencyList grid
--         graph = G.buildGraph l
--      in graph
--
-- buildAdjacencyList :: V.Vector (V.Vector Bool) -> [(Int, [Int])]
-- buildAdjacencyList grid = do
--     x <- [0 .. 127]
--     -- when building the graph, all edges are also reversed
--     y <- filter odd [0 .. 127]
--     guard $ not $ (grid ! y) ! x  -- used squares only
--     let i = y * 128 + x
--     let ns =
--             [ n
--             | (nx, ny) <- [(x-1, y), (x+1,y), (x, y-1), (x, y+1)]
--             , let n  = 128 * ny + nx
--             , ny >= 0 && ny < 128 && nx >= 0 && nx < 128
--             , not $ (grid ! ny) ! nx
--             ]
--     pure (i, ns)

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

grmbl :: A.Array (Int, Int) Status
grmbl = A.listArray ((0,0), (3,3)) $ concat
    [ [Used, Used, Free, Used]
    , [Free, Used, Free, Used]
    , [Free, Free, Free, Free]
    , [Used, Used, Used, Used]
    ]
-- grmbl = A.listArray ((0,0), (3,3)) [Used, Used, Free, Free, Free, Used, Free, Used, Free, Free, Free, Free, Used, Free, Used, Free]
