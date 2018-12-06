{-# LANGUAGE OverloadedStrings #-}

module Y2016.Day22 (answer1, answer2) where

import Control.Monad
import Control.Monad.Loops as Loops

import GHC.Word
import Data.Functor
import Data.Array as Arr
import Data.Void
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Text as Tx
import qualified Data.Text.IO as Tx.IO

type Parser = Parsec Void Tx.Text

type Coord = (Word16, Word16)
data Node = Node
    { nSize :: Word16
    , nUsed :: Word16
    }
    deriving (Show)
type Grid = Array Coord Node

answer1, answer2 :: IO Int
answer1 = do
    grid <- parseData
    let ns = Arr.assocs grid
    let viablePairs = do
            (x, nx) <- ns
            (y, ny) <- ns
            guard $ x /= y
            guard $ nUsed nx /= 0
            guard $ (nSize ny - nUsed ny) >= nUsed nx
            pure x
    pure $ length viablePairs


answer2 = error "wip"



parseData :: IO (Array Coord Node)
parseData = do
    raw <- Tx.IO.readFile "./data/2016/day22.txt"
    case parse dataParser "day22" raw of
      Left err -> print err *> error "failed"
      Right x -> pure x

dataParser :: Parser (Array Coord Node)
dataParser = do
    void $ takeLine *> char '\n'
    void $ takeLine *> char '\n'
    lines <- parseLine `Loops.untilM` isEOF
    let m = maximum $ fmap fst lines
    pure $ array ((0,0), m) lines

parseLine :: Parser (Coord, Node)
parseLine = do
    string "/dev/grid/node-x"
    x <- decimal
    string "-y"
    y <- decimal
    void takeSpaces
    size <- decimal
    char 'T'
    takeSpaces
    used <- decimal
    takeLine
    char '\n'
    let node = Node size used
    pure ((x, y), node)

takeLine :: Parser Tx.Text
takeLine = takeWhileP Nothing (/= '\n')

takeSpaces :: Parser Tx.Text
takeSpaces = takeWhileP Nothing (== ' ')

isEOF :: Parser Bool
isEOF = try (eof $> True) <|> pure False


aStar :: Coord -> Coord -> Array Coord Node -> Maybe [Coord]
aStar start goal grid = aStar' start goal grid Set.empty [(start, 0)] initScores
    where
        initScores
            = listArray (Arr.bounds grid) (repeat (Nothing, maxBound, maxBound))
            // [(start, (Nothing, 0, manhattan start goal))]


aStar'
    :: Coord
    -- ^ start
    -> Coord
    -- ^ goal
    -> Array Coord Node
    -- ^ original grid
    -> Set.Set Coord
    -- ^ closedSet (visited nodes)
    -> [(Coord, Word16)]
    -- ^ openSet (nodes to visit next, head is the most likely candidate)
    -> Array Coord (Maybe Coord, Word16, Word16)
    -- ^ array of (parentNode, startCost, totalCost)
    -- parentNode is used to reconstruct the path
    -- startCost: cost from start to this node
    -- totalCost: total cost from start to goal going through that node
    -> Maybe [Coord]
    -- ^ path from start to goal
aStar' _ _ _ _ [] _ = Nothing
aStar' current goal grid closedSet (x:xs) scores =
    let
    in error "wip"


manhattan :: Coord -> Coord -> Word16
manhattan (x1, y1) (x2, y2) =
    let x = if x2 > x1 then x2 - x1 else x1 - x2
        y = if y2 > y1 then y2 - y1 else y1 - y2
     in x + y

comeFrom (x, _, _) = x
fromStart (_, x, _) = x
totalScore (_, _, x) = x

insertOrd a [] = [a]
insertOrd a@(_, n) l@(x@(_, m):xs) =
    if n <= m
       then a : l
       else x : insertOrd a xs
