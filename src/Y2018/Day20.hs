{-# LANGUAGE LambdaCase #-}

module Y2018.Day20 (answer1, answer2) where

import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable
import Data.Maybe
import Data.Functor
import Data.List
import Data.Ord

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Control.Lens

type Parser = Parsec Void String


data Dir = N | S | W | E deriving (Show, Eq)
data Cell = Room | DoorH | DoorV | Wall deriving (Show, Eq)
type Point = (Int,Int)

newtype Regex = Regex [RegexSym] deriving (Show)

data RegexSym
  = D Dir
  | R [Regex]
  deriving (Show)


answer1, answer2 :: IO ()
answer1 = do
  regex <- getData
  print $ solve regex

answer2 = do
  regex <- getData
  let withDist = M.toList $ dijkstra (0,0) $ buildMap regex
  let res = length $ filter (\x -> snd x >= 1000) withDist
  print res

solve :: Regex -> Int
solve = head . sortOn Down . M.elems . dijkstra (0,0) . buildMap

buildMap :: Regex -> M.Map Point Cell
buildMap r =
  let s = (0,0)
      m0 = M.singleton s Room
      results = stepRegex (s, m0) r
   in foldl' (\m (_, m') -> M.union m' m) mempty results


stepRegex :: (Point, M.Map Point Cell) -> Regex -> [(Point, M.Map Point Cell)]
stepRegex (p, m) (Regex rSyms) = foldM stepSym (p, m) rSyms


stepSym :: (Point, M.Map Point Cell) -> RegexSym -> [(Point, M.Map Point Cell)]
stepSym (p, m) (D dir) =
  let (p', cells) = move p dir
      m' = foldl' (\m (k, v) -> M.insert k v m) m cells
   in [(p', m')]
stepSym start (R rs) =
  let withDup = concatMap (stepRegex start) rs
      f m (p, cells) = M.insertWith M.union p cells m
      deduped = foldl' f mempty withDup
   in M.toList deduped

move :: Point -> Dir -> (Point, [(Point, Cell)])
move (x,y) N = ((x,y-2), [((x,y-1), DoorH), ((x,y-2), Room)])
move (x,y) S = ((x,y+2), [((x,y+1), DoorH), ((x,y+2), Room)])
move (x,y) W = ((x-2,y), [((x-1,y), DoorV), ((x-2,y), Room)])
move (x,y) E = ((x+2,y), [((x+1,y), DoorV), ((x+2,y), Room)])


dijkstra
  :: Point -- starting point
  -> M.Map Point Cell -- map
  -> M.Map Point Int -- shortest number of door from start
dijkstra start map = go map S.empty M.empty [(start, 0)]
  where
    go _ _ distances [] = distances
    go !map !visited !distances ((p@(x,y), d):ps) =
      let neighbors = do
            a <- [x-1, x, x+1]
            b <- [y-1, y, y+1]
            guard $ (a,b) /= (x,y)
            let destX = 2 * a - x
            let destY = 2 * b - y
            let dest = (destX, destY)
            guard $ not (dest `S.member` visited)
            guard $ maybe False isDoor (M.lookup (a,b) map)
            guard $ maybe False isRoom (M.lookup dest map)
            pure (dest, d+1)
          f a b = if a < b then a else b
          distances' = foldl' (\m (x, d) -> M.insertWith f x d m) distances neighbors
          visited' = S.insert p visited
       in go map visited' distances' (neighbors <> ps)




isRoom Room = True
isRoom _ = False

isDoor DoorH = True
isDoor DoorV = True
isDoor _ = False

buildRegex :: String -> Regex
buildRegex raw = case parse parseRegex "regex" raw of
  Left err -> error $ show err
  Right r -> r

getData :: IO Regex
getData = buildRegex <$> readFile "data/2018/day20.txt"

parseRegex :: Parser Regex
parseRegex = do
  char '^'
  rs <- many parseRSym
  char '$'
  pure $ Regex rs


parseRSym :: Parser RegexSym
parseRSym
  = (D <$> parseDir)
  <|> do
    char '('
    subR <- (Regex <$> many parseRSym) `sepBy` char '|'
    char ')'
    pure $ R subR

parseDir :: Parser Dir
parseDir
  = char 'N' $> N
  <|> char 'S' $> S
  <|> char 'E' $> E
  <|> char 'W' $> W

prettyMap :: M.Map Point Cell -> String
prettyMap m =
  let ks = M.keys m
      minX = minimum $ fmap fst ks
      minY = minimum $ fmap snd ks
      maxX = maximum $ fmap fst ks
      maxY = maximum $ fmap snd ks
      makeLine y = [ pretty m (x,y) | x <- [minX..maxX]]
      ls = [makeLine y | y <- [minY..maxY]]
   in unlines ls

pretty m p
  | p == (0,0) = 'X'
  | otherwise = fromMaybe '#' $ pretty' <$> M.lookup p m

pretty' = \case
  Room -> '.'
  DoorH -> '−'
  DoorV -> '|'
  Wall -> '#'

prettyRegex (Regex syms) =
  let psym (D dir) = show dir
      psym (R rs) = "(" <> intercalate "|" (fmap pr rs) <> ")"
      pr (Regex syms) = concatMap psym syms
   in "^" <> concatMap psym syms <> "$"


test :: IO ()
test = do
  assertDist "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" 23
  assertDist "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"  31
  assertDist "^ENWWW(NEEE|SSE(EE|N))$" 10
  assertDist "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" 18
  assertDist "^N(ESNNW|N|NN)NNN$" 6
  raw <- head . lines <$> readFile "./data/2018/day20.txt"
  let dumped = prettyRegex (buildRegex raw)
  print $ dumped == raw
  print "done"

assertDist :: String -> Int -> IO ()
assertDist r expected = do
  let reg = buildRegex r
  when (prettyRegex reg /= r) $ do
    putStrLn "invalid parsing?"
    putStrLn r
    putStrLn (prettyRegex reg)
    print reg
  let d = solve reg
  if d == expected
     then putStrLn "ok"
     else do
       putStrLn r
       putStrLn $ prettyMap $ buildMap reg
       putStrLn $ "got: " <> show d <> " but expected: " <> show expected
