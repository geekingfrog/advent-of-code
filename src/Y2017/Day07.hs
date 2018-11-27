{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2017.Day07 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Ord
import Data.List as L
import Data.Maybe
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map

type Parser = Parsec Void Text

data Prog = Prog !Text !Int deriving Show
data Tree a = Node a [Tree a] deriving (Show, Functor)


answer1 :: IO Text
answer1 = findRoot <$> parseInput

answer2 :: IO Int
answer2 = do
    list <- parseInput
    let t = buildTree (findRoot list) list
    pure $ balance t 0


findRoot :: [(Prog, [Text])] -> Text
findRoot list =
    let allNames = [n | (Prog n _, _) <- list]
        allChildren = concatMap snd list
        root = Set.difference (Set.fromList allNames) (Set.fromList allChildren)
    in case Set.toList root of
         [x] -> x
         _ -> error "invalid input"

buildTree :: Text -> [(Prog, [Text])] -> Tree Prog
buildTree rootName rawNodes =
    let mappings = Map.fromList $ map (\p@(Prog n _, _) -> (n, p)) rawNodes
        buildTree' m root =
            let (n, cs) = fromMaybe (error "cannot find node") (Map.lookup root m)
                children = fmap (buildTree' m) cs
             in Node n children
      in buildTree' mappings rootName

weight :: Tree Prog -> Int
weight (Node (Prog _ n) cs) = n + sum (fmap weight cs)

balance :: Tree Prog -> Int -> Int
balance (Node (Prog _ n) []) delta = n + delta
balance (Node (Prog _ n) cs) delta =
    let branches = fmap (\c -> (weight c, c)) cs
        okBranches = maximumBy (comparing length) $ L.groupBy (\a b -> fst a == fst b) branches
        okWeight = fst $ head okBranches
        unbalancedBranch = find ((/= okWeight) . fst) branches
     in case unbalancedBranch of
          Nothing -> n + delta -- children balanced, so need to adjust that node
          Just b -> balance (snd b) (okWeight - fst b)



parseInput :: IO [(Prog, [Text])]
parseInput = do
    raw <- T.readFile "data/2017/day07.txt"
    case parse progs "day07" raw of
      Left e -> error $ "parse error " ++ show e
      Right stuff -> pure stuff

progs :: Parser [(Prog, [Text])]
progs = sepEndBy progLine newline

progLine = do
    name <- progName
    space
    cost <- read <$> between (char '(') (char ')') (many digitChar)
    c <- children
    pure (Prog name cost, c)

children :: Parser [Text]
children = try (do
    space
    string "->"
    space
    sepBy progName (char ',' >> space)) <|> pure []

progName = T.pack <$> many letterChar
