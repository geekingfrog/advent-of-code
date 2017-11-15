module Y2016.Day11 (answer1, answer2) where

import Control.Monad
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Sequence ((<|), (|>), Seq(..))
import qualified Data.Sequence as Seq
import Data.Monoid

import Debug.Trace as Debug

answer1, answer2 :: Int
answer1 = solve initialState

answer2 = 42


solve :: Floors -> Int
solve start = go Set.empty (Seq.singleton (0, start))
  where
    go :: Set.Set Floors -> Seq.Seq (Int, Floors) -> Int
    go _ Seq.Empty = error "no solution?"
    go visited ((n, s):<|rest)
        | finalState s
        = n
        | otherwise
        = let nexts    = filter (`Set.notMember`visited) (nextStates s)
              visited' = Set.union visited (Set.fromList nexts)
              seq'     = rest <> Seq.fromList (map (\x -> (n+1, x)) nexts)
          in  go visited' seq'


data Element = Tm | Sr | Pu | Ru | Pm
    deriving (Eq, Show, Ord)
-- Thulium, Strontium, Plutonium, Ruthenium, Promethium

data Tech = Gen Element | Chip Element deriving (Show, Eq, Ord)


type Floor = [Tech]
type Floors = (Int, V.Vector Floor)
            -- ^- elevator position
            --       ^- list of floors


initialState :: Floors
initialState =
    let f0 = [Gen Tm, Chip Tm, Gen Pu, Gen Sr]
        f1 = [Chip Pu, Chip Sr]
        f2 = [Gen Pm, Chip Pm, Gen Ru, Chip Ru]
        f3 = []
    in  (0, V.fromList [f0, f1, f2, f3])

testState :: Floors
testState =
    let f0 = [Chip Tm, Chip Sr]
        f1 = [Gen Tm]
        f2 = [Gen Sr]
        f3 = []
    in  (0, V.fromList [f0, f1, f2, f3])

safeFloor :: Floor -> Bool
safeFloor f = all (safeTech f) f
  where
    safeTech []    _         = True
    safeTech _     (Gen  _ ) = True
    safeTech techs (Chip el) = Gen el `elem` techs || not (hasGen techs)
    hasGen []         = False
    hasGen (Gen _:_ ) = True
    hasGen (_    :ts) = hasGen ts

safeState :: Floors -> Bool
safeState (_, floors) = all safeFloor floors

finalState :: Floors -> Bool
finalState (n, floors) =
    let lastFloor       = V.last floors
        otherFloors     = V.init floors
    in  n == 3 && all null (V.init floors)


nextStates :: Floors -> [Floors]
nextStates (n, floors) = do
    n' <- [n + 1, n - 1]
    guard $ n' >= 0
    guard $ n' <= 3
    let currentFloor = floors V.! n
    let nextFloor    = floors V.! n'
    itemTaken <- subSeqs currentFloor
    let remaining      = filter (`notElem`itemTaken) currentFloor
    let withAddedItems = nextFloor ++ itemTaken
    let floors'        = floors V.// [(n, remaining), (n', withAddedItems)]
    let nextState      = (n', floors')
    guard $ safeFloor remaining
    guard $ safeFloor withAddedItems
    pure nextState


subSeqs :: (Ord a, Eq a) => [a] -> [[a]]
subSeqs l = [[x] | x <- l] ++ [[x, y] | x <- l, y <- l, x < y]


pretty :: Floors -> String
pretty (n, floors) =
    let
        prefix i = "F" ++ show i ++ " " ++ if i == (n+1) then " E " else " . "
        fs = map (\(i, f) -> prefix i ++ " " ++ prettyFloor f) (zip [1..] $ V.toList floors)
    in
        unlines $ reverse fs

prettyFloor :: Floor -> String
prettyFloor techs =
    let
      order = [c e | e <- [Tm, Sr, Pu, Ru, Pm], c <- [Gen, Chip]]
      prettyShow (Gen x) = show x ++ "G"
      prettyShow (Chip x) = show x ++ "M"
      strs = [if x `elem` techs then prettyShow x else " . " | x <- order]
    in
      unwords strs

-- raw puzzle input below
-- The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
-- The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
-- The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
-- The fourth floor contains nothing relevant.

-- F4 .  .  .  .  .
-- F3 E  HG HM LG .
-- F2 .  .  .  .  .
-- F1 .  .  .  .  LM
