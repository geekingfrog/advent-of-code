{-# LANGUAGE BangPatterns #-}

module Y2017.Day22 (answer1, answer2) where

import Control.Monad
import Data.Foldable
import qualified Data.HashMap.Strict as Map

answer1, answer2 :: IO ()
answer1 = do
    grid <- parseInput
    let initialState = ((0,-1), (0,0), grid, 0)
    let n = 10000
    let (_, _, _, c) = foldl' (\s _ -> step s) initialState [1..n]
    print c

answer2 = do
    grid <- parseInput
    let initialState = ((0,-1), (0,0), grid, 0)
    let n = 10000000
    let (_, _, _, c) = foldl' (\s _ -> step2 s) initialState [1..n]
    print c

data Status = Clean | Infected | Weakened | Flagged deriving Show
type Coord = (Int, Int)
type Grid = Map.HashMap Coord Status
type Direction = Coord

step :: (Direction, Coord, Grid, Int) -> (Direction, Coord, Grid, Int)
step (dir, pos, grid, !counter) =
    let node = Map.lookupDefault Clean pos grid
        dir' = case node of
                 Infected -> turnRight dir
                 Clean -> turnLeft dir
                 _ -> dir
        node' = switchStatus node
        grid' = Map.insert pos node' grid
        pos' = pos +: dir'
        counter' = if isInfected node' then counter + 1 else counter
     in (dir', pos', grid', counter')


switchStatus Infected = Clean
switchStatus Clean = Infected
switchStatus x = x

isInfected Infected = True
isInfected _ = False


step2 :: (Direction, Coord, Grid, Int) -> (Direction, Coord, Grid, Int)
step2 (dir, pos, grid, !counter) =
    let node = Map.lookupDefault Clean pos grid
        dir' = case node of
                 Clean -> turnLeft dir
                 Weakened -> dir
                 Infected -> turnRight dir
                 Flagged -> turnRight (turnRight dir)
        node' = switchStatus2 node
        grid' = Map.insert pos node' grid
        pos' = pos +: dir'
        counter' = if isInfected node' then counter + 1 else counter
     in (dir', pos', grid', counter')


switchStatus2 Clean = Weakened
switchStatus2 Weakened = Infected
switchStatus2 Infected = Flagged
switchStatus2 Flagged = Clean


turnLeft (x, y) = (y, -x)
turnRight (x, y) = (-y, x)

(a, b) +: (c, d) = (a + c, b + d)

parseInput = do
    rows <- lines <$> readFile "./data/2017/day22.txt"
    let cy          = (length rows - 1) `div` 2
    let cx          = (length (head rows) - 1) `div` 2
    let rowsWithIdx = zip [-cy ..] rows

    let as =
            [ ((x, y), c)
            | (y, row) <- rowsWithIdx
            , (x, k  ) <- zip [-cx ..] row
            , let c = if k == '#' then Infected else Clean
            ]
    pure $ Map.fromList as


testInput :: Grid
testInput = Map.fromList [((1,-1), Infected), ((-1,0), Infected)]
