{-# LANGUAGE TupleSections #-}

module Y2016.Day11 (answer1, answer2) where

import Control.Monad
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Sequence ((<|), (|>), Seq(..))
import qualified Data.Sequence as Seq
import Data.Monoid

import GHC.Word
import Data.Bits as Bits

import Debug.Trace as Debug

answer1, answer2 :: Int
answer1 = solve initialState -- 31 in 4s
answer2 = solve initialState2 -- 55 in 253s


solve :: Building -> Int
solve start = go Set.empty (Seq.singleton (0, start))
  where
    go :: Set.Set Building -> Seq.Seq (Int, Building) -> Int
    go _ Seq.Empty = error "no solution?"
    go visited ((n, s):<|rest)
        | finalState s
        = n
        | otherwise
        = let nexts    = filter (`Set.notMember`visited) (nextStates s)
              visited' = Set.union visited (Set.fromList nexts)
              seq'     = rest <> Seq.fromList (fmap (n+1, ) nexts)
          in  go visited' seq'
    go _ _ = error "impossible!"




type Floor = (Word8, Word8) -- chips, generators
type Building = (Int, V.Vector Floor) -- (elevator position, floors)

-- elements: Tm, Sr, Pu, Ru, Pm, El, Di
--           1 , 2 , 4 , 8 , 16, 32, 64

safeFloor :: Floor -> Bool
safeFloor (chips, gens) = chips == 0 || gens == 0 || (chips .&. gens) == chips

nextStates :: Building -> [Building]
nextStates (n, floors) = do
    n' <- [n + 1, n - 1]
    guard $ n' >= 0 && n' < 4
    let currentFloor@(currentChips, currentGens) = floors V.! n
    let (nextChips, nextGens) = floors V.! n'
    (cs, gs) <- possibleItemsToTake currentFloor
    let currentFloor' = (currentChips `xor` cs, currentGens `xor` gs)
    let nextFloor' = (nextChips .|. cs, nextGens .|. gs)
    guard $ safeFloor currentFloor' && safeFloor nextFloor'
    let updated = floors V.// [(n, currentFloor'), (n', nextFloor')]
    pure (n', updated)


elements :: [Word8]
elements = [shiftL 1 n | n <- [0..6]]

possibleItemsToTake :: Floor -> [Floor]
possibleItemsToTake (chips, gens) =
    let cs = [c | c <- elements, (chips .&. c) /= 0]
        gs = [g | g <- elements, (gens .&. g) /= 0]
        singleChip = [(c, 0) | c <- cs]
        singleGen = [(0, g) | g <- gs]
        dblChip = [(c1 + c2, 0) | c1 <- cs, c2 <- cs, c2 > c1]
        dblGen = [(0, g1 + g2) | g1 <- gs, g2 <- gs, g2 > g1]
        mixed = [(c, g) | c <- cs, g <- gs]
     in singleChip ++ singleGen ++ dblChip ++ dblGen ++ mixed

finalState :: Building -> Bool
finalState (n, floors) = n == 3 && all (== (0,0)) (V.init floors)

initialState, initialState2, testState :: Building
initialState =
    let f0 = (1, 1 + 2 + 4)
        f1 = (2 + 4, 0)
        f2 = (8 + 16, 8 + 16)
        f3 = (0, 0)
    in  (0, V.fromList [f0, f1, f2, f3])

initialState2 =
    let f0 = (1 + 32 + 64, 1 + 2 + 4 + 32 + 64)
        f1 = (2 + 4, 0)
        f2 = (8 + 16, 8 + 16)
        f3 = (0, 0)
    in  (0, V.fromList [f0, f1, f2, f3])

testState =
    let f0 = (1 + 2, 0)
        f1 = (0, 1)
        f2 = (0, 2)
        f3 = (0, 0)
    in  (0, V.fromList [f0, f1, f2, f3])


-- raw puzzle input below
-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.

-- F4 .  .  .  .  .
-- F3 E  HG HM LG .
-- F2 .  .  .  .  .
-- F1 .  .  .  .  LM
