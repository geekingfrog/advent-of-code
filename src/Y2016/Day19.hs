module Y2016.Day19 (answer2) where

import Data.Sequence


answer2 :: Int
answer2 =
    let n = 3018458
        start = fromList [1..n]
     in index (foldl (\t _ -> steal t) start [1..n-1]) 0


steal :: Seq Int -> Seq Int
-- first elf on the table steal present
steal table@(e :<| es) =
    let target = Data.Sequence.length table `div` 2 - 1
        table' = deleteAt target es |> e
     in table'
steal _ = error "empty sequence"
