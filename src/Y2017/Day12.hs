module Y2017.Day12 (answer1, answer2, buildGraph, findGroups, Graph) where

import Data.Maybe
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.IntMap as Map
import qualified Data.IntSet as Set

import Text.Megaparsec
import Text.Megaparsec.Text

answer1, answer2 :: IO Int
answer1 = do
    parsed <- parseInput
    let graph = buildGraph parsed
    pure $ Set.size $ descendants graph 0
answer2 = do
    parsed <- parseInput
    let graph = buildGraph parsed
    pure $ length $ findGroups graph


type Graph = Map.IntMap [Int]


descendants :: Graph -> Int -> Set.IntSet
descendants graph start = go Set.empty [start] graph
  where
    go s [] _ = s
    go s (x:xs) g =
        let neighbors    = fromMaybe [] $ Map.lookup x g
            newNeighbors = filter (not . (`Set.member`s)) neighbors
            s'           = foldl (flip Set.insert) s neighbors
        in  go s' (newNeighbors ++ xs) g

findGroups :: Graph -> [Set.IntSet]
findGroups g =
    let nodes = Map.keys g
    in  case nodes of
            [] -> []
            (x:xs) ->
                let group = descendants g x
                    g'    = foldl' (flip Map.delete) g (Set.toList group)
                in  group : findGroups g'


buildGraph :: [(Int, [Int])] -> Graph
buildGraph = foldl' folder Map.empty
  where
    folder m (n, ns) =
      let mDirect = Map.insertWith (++) n ns m
          -- add the other direction as well
          mReverse = foldl' (\m x -> Map.insertWith (++) x [n] m) mDirect ns
      in mReverse

parseInput :: IO [(Int, [Int])]
parseInput = do
    raw <- T.readFile "data/2017/day12.txt"
    case parse parseConnections "input" raw of
        Left err -> error $ show err
        Right x -> pure x

parseConnections :: Parser [(Int, [Int])]
parseConnections = parseConnection `sepEndBy` newline

parseConnection = do
    n <- parseInt
    string " <-> "
    ns <- parseInt `sepBy` string ", "
    pure (n, ns)

parseInt = read <$> some digitChar

test :: [(Int, [Int])]
test =
    [ (0, [2])
    , (1, [1])
    , (2, [0, 3, 4])
    , (3, [2, 4])
    , (4, [2, 3, 6])
    , (5, [6])
    , (6, [4, 5])
    ]
