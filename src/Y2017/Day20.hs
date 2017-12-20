{-# LANGUAGE BangPatterns #-}

module Y2017.Day20 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Foldable
import Data.Ord
import Data.Function
import Data.List

import qualified Data.HashSet as Set

type Coord = (Int, Int, Int)
type Particule = (Coord, Coord, Coord)
-- x(t) = x0 + t*v0 + (t * (t+1)) / 2 * p0

answer1, answer2 :: IO Int
answer1 = do
    ps <- parseInput
    let withIdx = zip [0..] ps
    pure $ fst $ minimumBy (compare `on` (absCoord . third . snd)) withIdx

answer2 = do
    ps <- parseInput
    let allParticules = iterate (removeCollided . fmap step) ps
    -- 100 is arbitrarily chosen, not super clean but it works™
    pure $ length $ allParticules !! 100

first (x, _, _) = x
third (_, _, x) = x
absCoord (x, y, z) = abs x + abs y + abs z


(a1, b1, c1) +: (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


steps :: [Particule] -> [Particule]
steps = fmap step

step :: Particule -> Particule
step (x, y, z) = (x +: y +: z, y +: z, z)

removeCollided :: [Particule] -> [Particule]
removeCollided ps =
    let collidedPos = findCollided ps
     in filter (not . (`Set.member` collidedPos) . first) ps

findCollided :: [Particule] -> Set.HashSet Coord
findCollided = snd . foldl' go (Set.empty, Set.empty)
  where
    go (!seenPos, !dups) p = if Set.member (first p) seenPos
        then (seenPos, Set.insert (first p) dups)
        else (Set.insert (first p) seenPos, dups)


parseInput = do
    raw <- readFile "./data/2017/day20.txt"
    case parse (parseLine `sepEndBy` newline) "day 20" raw of
      Left err -> error $ show err
      Right ps -> pure ps

parseLine :: Parser Particule
parseLine = do
    string "p="
    p <- between (char '<') (char '>') parseCoord
    string ", v="
    v <- between (char '<') (char '>') parseCoord
    string ", a="
    a <- between (char '<') (char '>') parseCoord
    pure (p, v, a)

parseCoord = (,,) <$> parseDigit <*> (char ',' *> parseDigit) <*> (char ',' *> parseDigit)

parseDigit :: Parser Int
parseDigit = do
    sign <- optional (char '-')
    d    <- some digitChar
    pure $ case sign of
        Nothing -> read d
        Just _  -> negate $ read d


test =
    [ ((-6, 0, 0), (3, 0, 0) , (0, 0, 0))
    , ((-4, 0, 0), (2, 0, 0) , (0, 0, 0))
    , ((-2, 0, 0), (1, 0, 0) , (0, 0, 0))
    , ((3, 0, 0) , (-1, 0, 0), (0, 0, 0))
    ]
