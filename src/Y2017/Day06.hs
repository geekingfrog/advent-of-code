module Y2017.Day06 (answer1, answer2) where

import Data.Vector ((!))
import qualified Data.Vector as V
import qualified Data.Set as Set

answer1, answer2 :: IO ()
answer1 = print $ loop 0 Set.empty $ iterate balance input
answer2 = let allBalances = iterate balance input
              start = loop 0 Set.empty allBalances
           in print $ loop 0 Set.empty (drop start allBalances)

balance :: V.Vector Int -> V.Vector Int
balance v =
    let n       = V.length v
        k       = V.maxIndex v
        m       = v ! k
        updates = do
            i <- [0 .. n - 1]
            let dist = if i <= k then n - k + i else i - k
            let delta  = 1 + (m - dist) `div` n
            let val = if i == k then 0 else v ! i
            pure (i, val + delta)
     in  v V.// updates


loop :: Int -> Set.Set (V.Vector Int) -> [V.Vector Int] -> Int
loop i s (x:xs) =
    if Set.member x s then i else loop (i + 1) (Set.insert x s) xs
loop _ _ _ = error "impossible"


input, test :: V.Vector Int
input = V.fromList [4, 1, 15, 12, 0, 9, 9, 5, 5, 8, 7, 3, 14, 5, 12, 3]
test = V.fromList [0,2,7,0]
