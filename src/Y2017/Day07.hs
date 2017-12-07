module Y2017.Day07 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.Text

import Data.Text
import Data.Text.IO as T

data Prog = Prog Text Int deriving Show
data Tree = Leaf Prog | Node Prog [Tree] deriving Show


answer1, answer2 :: IO Text
answer1 = error "wip 1"
answer2 = error "wip 2"


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

progName = pack <$> many letterChar
