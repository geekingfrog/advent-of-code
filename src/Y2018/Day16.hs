{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Y2018.Day16 (answer1, answer2) where

import           Control.Monad
import           Control.Monad.Loops           as Loops
import           Control.Lens

import           Data.Foldable
import           GHC.Word
import           Data.Void
import           Data.Bits
import           Data.Maybe
import           Data.List
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!) )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Utils.Parser                  as U

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

type Parser = Parsec Void Tx.Text

data Reg = RA | RB | RC | RD
data Val = V (CPU -> Word16) | R !Reg
type CPU = (Word16, Word16, Word16, Word16)


answer1, answer2 :: IO ()
answer1 = do
  (samples, program) <- getData
  let stuff = fmap (\s -> filter (matchSample s) instructions) samples
  let lessStuff = filter ((>= 3) . length) stuff
  print $ length lessStuff

answer2 = do
  (samples, program) <- getData
  let ops = mapOpCodes samples
  let finalState = foldl' (execOpCode ops) (0, 0, 0, 0) program
  print $ finalState ^. _1

swap (a, b) = (b, a)

matchSample s i =
  let (_, a, b, c) = instruction s
  in exec i a b c (beforeS s) == Just (afterS s)

execOpCode :: V.Vector Ins -> CPU -> Instruction -> CPU
execOpCode ops s (o, a, b, c) =
  let i = ops ! fromIntegral o
  in fromMaybe (error "wut?") $ exec i a b c s

mapOpCodes :: [Sample] -> V.Vector Ins
mapOpCodes samples =
  case findAllOpCodes 16 samples of
    [(mapping, _)] ->
      let rev = Map.fromList $ swap <$> Map.toList mapping
          f i = let (Just ins) = Map.lookup i rev in ins
          ops = V.generate 16 (f . fromIntegral)
      in ops
    [] -> error "no assignment ???"
    _ -> error "multiple assignments ???"

findAllOpCodes :: Int -> [Sample] -> [(Map.Map Ins Word16, Execs)]
findAllOpCodes n samples = foldM mapOpCode (mempty, fmap mapSample samples) [1..n]

type Execs = [(Word16, [Ins])]

mapOpCode :: (Map.Map Ins Word16, Execs) -> a -> [(Map.Map Ins Word16, Execs)]
mapOpCode (known, execs) _ =
  let
      vals = Map.elems known
      f :: (Word16, [Ins]) -> (Word16, [Ins])
      f (o, is) = (o, filter (\i -> not (i `Map.member` known) && o `notElem` vals) is)
      trimmed = filter (not . null . snd) $ fmap f execs
      sorted = sortOn (length . snd) trimmed
      res = case sorted of
        [] -> []
        ((o, is):_) -> [(Map.insert i o known, trimmed) | i <- is]
  in res

mapSample :: Sample -> (Word16, [Ins])
mapSample sample =
  let (o, a, b, c) = instruction sample
      f i = exec i a b c (beforeS sample) == Just (afterS sample)
  in (o, filter f instructions)

getVal x st = Just x

getReg x st = case x of
  0 -> Just $ st ^. _1
  1 -> Just $ st ^. _2
  2 -> Just $ st ^. _3
  3 -> Just $ st ^. _4
  _ -> Nothing

getTarget t = case t of
  0 -> Just _1
  1 -> Just _2
  2 -> Just _3
  3 -> Just _4
  _ -> Nothing

binOp
  :: (Word16 -> Word16 -> Word16)
  -> (Word16 -> CPU -> Maybe Word16)
  -> (Word16 -> CPU -> Maybe Word16)
  -> Word16
  -> Word16
  -> Word16
  -> CPU
  -> Maybe CPU
binOp op getS0 getS1 s0 s1 target st = do
  a <- getS0 s0 st
  b <- getS1 s1 st
  t <- getTarget target
  pure $ st & t .~ op a b


unOp op getS = binOp (\a b -> op a) getS getVal

data Ins
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Show, Eq, Bounded, Enum, Ord)

exec :: Ins -> Word16 -> Word16 -> Word16 -> CPU -> Maybe CPU
exec = \case
  Addr -> binOp (+) getReg getReg
  Addi -> binOp (+) getReg getVal
  Mulr -> binOp (*) getReg getReg
  Muli -> binOp (*) getReg getVal
  Banr -> binOp (.&.) getReg getReg
  Bani -> binOp (.&.) getReg getVal
  Borr -> binOp (.|.) getReg getReg
  Bori -> binOp (.|.) getReg getVal
  Setr -> unOp id getReg
  Seti -> unOp id getVal
  Gtir -> binOp (\a b -> if a > b then 1 else 0) getVal getReg
  Gtri -> binOp (\a b -> if a > b then 1 else 0) getReg getVal
  Gtrr -> binOp (\a b -> if a > b then 1 else 0) getReg getReg
  Eqir -> binOp (\a b -> if a == b then 1 else 0) getVal getReg
  Eqri -> binOp (\a b -> if a == b then 1 else 0) getReg getVal
  Eqrr -> binOp (\a b -> if a == b then 1 else 0) getReg getReg


instructions = [minBound..maxBound]

data Sample = Sample
  { beforeS :: CPU
  , afterS :: CPU
  , instruction :: (Word16, Word16, Word16, Word16)
  }
  deriving (Show)

-- opcode, a, b, c
type Instruction = (Word16, Word16, Word16, Word16)

getData :: IO ([Sample], [Instruction])
getData = do
  raw <- Tx.IO.readFile "data/2018/day16.txt"
  case parse parseData "day16" raw of
    Left err -> error $ show err
    Right x -> pure x

parseData :: Parser ([Sample], [Instruction])
parseData = (,)
  <$> parseSamples
  <* char '\n' <* char '\n'
  <*> parseProgram

parseSamples :: Parser [Sample]
parseSamples = parseWhile (parseSample <* char '\n')

parseWhile p = Loops.unfoldM (try (Just <$> p) <|> pure Nothing)

parseSample :: Parser Sample
parseSample = do
  s0 <- string "Before: " *> parseCPU <* char '\n'
  i <- parseInstruction <* char '\n'
  s1 <- string "After:  " *> parseCPU <* char '\n'
  pure $ Sample s0 s1 i

parseProgram :: Parser [Instruction]
parseProgram = U.parseLines parseInstruction

parseCPU :: Parser CPU
parseCPU = do
  char '['
  r0 <- decimal
  string ", "
  r1 <- decimal
  string ", "
  r2 <- decimal
  string ", "
  r3 <- decimal
  char ']'
  pure (r0, r1, r2, r3)

parseInstruction = do
  opCode <- decimal
  char ' '
  a <- decimal
  char ' '
  b <- decimal
  char ' '
  c <- decimal
  pure (opCode, a, b, c)


test :: IO ()
test = do
  let s0 = (3, 2, 1, 1)
  print $ filter ((== Just (3,2,2,1)) . snd) $ fmap (\i -> (i, exec i 2 1 2 s0)) instructions
