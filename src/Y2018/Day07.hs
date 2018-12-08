{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day07 (answer1, answer2) where

import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Utils.Parser as Utils

import qualified Data.Array as Arr
import Control.Monad
import Control.Monad.State as St
import Control.Monad.Loops
import Data.Foldable
import Data.Void

import Data.Char as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List


type Parser = Parsec Void Tx.Text
type Gr = Map.Map Char String

answer1, answer2 :: IO ()
answer1 = solve . buildGraphs <$> getData >>= print
answer2 = getData >>= print . ts . buildSleigh . buildGraphs

solve :: (Gr, Gr) -> String
solve (deps, revDeps)
  = let start = findStart deps revDeps
        states = iterate (step deps revDeps) ("", start, mempty)
        final = find (\(_, xs, _) -> null xs) states
     in maybe (error "wut?") (\(x, _, _) -> reverse x) final

step :: Gr -> Gr -> (String, String, Set.Set Char) -> (String, String, Set.Set Char)
step deps revDeps a@(acc, toDo, done) =
  case toDo of
    [] -> a
    (x:xs) | x `Set.member` done -> (acc, xs, done)
           | not (canBeDone revDeps done x) -> step deps revDeps (acc, xs <> [x], done)
           | otherwise -> let done' = Set.insert x done
                              candidates = filter (not . (`Set.member` done')) $ Map.findWithDefault [] x deps
                              (nexts, later) = partition (canBeDone revDeps done') candidates
                              toDo' = nub $ sort (nexts <> xs) <> sort later
                           in (x:acc, toDo', done')


canBeDone :: Gr -> Set.Set Char -> Char -> Bool
canBeDone revDeps done x = all (`Set.member` done) (Map.findWithDefault [] x revDeps)

findStart :: Gr -> Gr -> String
findStart deps revDeps
  = let starts = map fst $ filter (\(k, d) -> null d) $ Map.assocs revDeps
        valid = filter (\k -> not $ null $ Map.findWithDefault [] k deps) starts
    in case valid of
      [] -> error "no starting points???"
      xs -> sort xs


data BuildState = BuildState
  { workers :: [(Char, Int)] -- piece worked on, completion time
  , done :: Set.Set Char
  , toDo :: String
  , ts :: Int
  }
  deriving (Show)

numWorkers = 5

initState steps = BuildState
  { workers = []
  , done = mempty
  , toDo = steps
  , ts = -1
  }

buildSleigh :: (Gr, Gr) -> BuildState
buildSleigh gs@(_, revDeps)
  = let steps = solve gs
        initSt = initState steps
        finalState = St.evalState (iterateUntil sleighIsDone (buildSleighStep revDeps)) initSt
    in finalState


tmp :: (Gr, Gr) -> [BuildState]
tmp gs@(_, revDeps)
  = let steps = solve gs
        initSt = initState steps
        states = St.evalState (buildSleighStep revDeps `untilM` St.gets sleighIsDone) initSt
    in states


-- going full stateful because I have no idea how to do that otherwise -_-
buildSleighStep :: Gr -> St.State BuildState BuildState
buildSleighStep revDeps = do
  St.modify completeWork
  st <- St.get
  case findWork revDeps st of
    [] -> pure ()
    cs -> do
      let items = map (\c -> (c, C.ord c - 65 + 60 + 1 + ts st)) cs
      St.put $ foldl' (flip (assignWorkers numWorkers)) st items
  St.modify tick
  St.get

sleighIsDone :: BuildState -> Bool
sleighIsDone st = null (toDo st) && null (workers st)

tick :: BuildState -> BuildState
tick st = st {ts = ts st + 1}

assignWorkers :: Int -> (Char, Int) -> BuildState -> BuildState
assignWorkers n item st = if length (workers st) == n
  then st
  else st { workers = item : workers st
          , toDo = filter (/= fst item) (toDo st)
          }

findWork :: Gr -> BuildState -> String
findWork revDeps st = filter (canBeDone revDeps (done st)) (toDo st)

completeWork :: BuildState -> BuildState
completeWork st
  = let (completed, rest) = partition (\x -> ts st >= snd x) (workers st)
    in st { workers = rest
          , done = foldl' (\s (c, _) -> Set.insert c s) (done st) completed
          }

buildGraphs :: [(Char, Char)] -> (Gr, Gr)
buildGraphs edges = (buildDeps edges, buildRevDeps edges)

buildDeps :: [(Char, Char)] -> Gr
buildDeps edges =
  foldl'
    (\m (k, v) -> Map.insertWith (<>) k v m)
    (Map.fromList (map ((, []) . fst) edges))
    (map (\(a, b) -> (a, [b])) edges)

buildRevDeps :: [(Char, Char)] -> Gr
buildRevDeps edges =
  foldl'
    (\m (k, v) -> Map.insertWith (<>) k v m)
    (Map.fromList (map ((, []) . fst) edges))
    (map (\(a, b) -> (b, [a])) edges)

getData :: IO [(Char, Char)]
getData = do
  raw <- Tx.IO.readFile "./data/2018/day07.txt"
  case parse parseData "day07" raw of
    Right x -> pure x
    Left err -> error $ show err

parseData :: Parser [(Char, Char)]
parseData = Utils.parseLines parseLine

parseLine :: Parser (Char, Char)
parseLine
  = (,)
  <$ string "Step "
  <*> upperChar
  <* string " must be finished before step "
  <*> upperChar
  <* string " can begin."
