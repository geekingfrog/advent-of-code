module Y2017.Day20 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.String
import Data.Foldable
import Data.Ord
import Data.Function
import Data.List

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
    mapM_ (putStrLn . show . length) (steps ps)
    error "wip"

-- 574 too high

first (x, _, _) = x
third (_, _, x) = x
absCoord (x, y, z) = abs x + abs y + abs z


(a1, b1, c1) `plus` (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)
x `mul` (a, b, c) = (x*a, x*b, x*c)


pos :: Int -> Particule -> Coord
pos t (a, v, p) = a `plus` (t `mul` v) `plus` (((t * (t+1)) `div` 2) `mul` p)


steps = go 0
  where
    go i ps =
        let ps' = step i ps
         in ps : go (i+1) ps'

step :: Int -> [Particule] -> [Particule]
step i ps =
    let withPos = fmap (\p -> (pos i p, p)) ps
     in snd <$> nubBy ((==) `on` fst) withPos


parseInput = do
    raw <- readFile "./data/2017/day20.txt"
    case parse (parseLine `sepEndBy` newline) "day 20" raw of
      Left err -> error $ show err
      Right ps -> pure ps

parseLine :: Parser Particule
parseLine = do
    string "p="
    p <- between (char '<') (char '>') parseTriple
    string ", v="
    v <- between (char '<') (char '>') parseTriple
    string ", a="
    a <- between (char '<') (char '>') parseTriple
    pure (p, v, a)

parseTriple = (,,) <$> parseDigit <*> (char ',' *> parseDigit) <*> (char ',' *> parseDigit)

parseDigit :: Parser Int
parseDigit = do
    sign <- optional (char '-')
    d    <- some digitChar
    pure $ case sign of
        Nothing -> read d
        Just _  -> negate $ read d
