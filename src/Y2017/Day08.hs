{-# LANGUAGE OverloadedStrings #-}

module Y2017.Day08 (answer1, answer2) where

import Data.Functor
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Foldable
import Data.HashMap.Strict as Map

type Parser = Parsec Void Text

answer1, answer2 :: IO ()
answer1 = maximum . Map.elems . foldl eval Map.empty <$> parseInput >>= print

answer2 = do
    instructions <- parseInput
    let allStates = scanl eval Map.empty instructions
    print $ maximum $ concatMap Map.elems allStates


type Register = Text
type Env = Map.HashMap Register Int
data Cond = Cond Register (Int -> Bool)
data Ins = Ins Register (Int -> Int) Cond


eval :: Env -> Ins -> Env
eval env (Ins r op (Cond r' c)) =
    let rv  = Map.lookupDefault 0 r env
        rv' = Map.lookupDefault 0 r' env
    in  if c rv' then Map.insert r (op rv) env else env


parseInput :: IO [Ins]
parseInput = do
    raw <- T.readFile "data/2017/day08.txt"
    case parse parseInstructions "day 08" raw of
        Left  err          -> error $ "parse error: " ++ show err
        Right instructions -> pure instructions

parseInstructions :: Parser [Ins]
parseInstructions = sepEndBy parseInstruction newline

parseInstruction :: Parser Ins
parseInstruction = do
    name <- T.pack <$> some letterChar
    space
    op <- (string "dec" >> pure (-)) <|> (string "inc" $> (+))
    space
    n <- parseNum
    space
    string "if"
    space
    reg' <- T.pack <$> some letterChar
    space
    cond <- parseCond
    space
    k <- parseNum
    pure $ Ins name (`op`n) (Cond reg' (`cond`k))

parseNum :: Parser Int
parseNum = do
    sign <- optional (char '-')
    n    <- read <$> some digitChar
    let s' = maybe id (const negate) sign
    pure $ s' n

parseCond :: (Ord a, Eq a) => Parser (a -> a -> Bool)
parseCond =
    (string "<=" $> (<=))
        <|> (string ">=" $> (>=))
        <|> (string ">" $> (>))
        <|> (string "<" $> (<))
        <|> (string "==" $> (==))
        <|> (string "!=" $> (/=))
