{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Y2018.Day19 (answer1, answer2) where

import           Control.Monad
import           Control.Monad.Loops           as Loops
import           Control.Lens

import           Data.Foldable
import           GHC.Word
import           Data.Void
import           Data.Bits
import           Data.Maybe
import           Data.Functor
import           Data.List
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!), (//), (!?) )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Utils.Parser                  as U

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

type Parser = Parsec Void Tx.Text

type Registers = V.Vector Word32
data CPU = CPU
  { ip :: Int
  , ipVal :: Int
  , regs :: Registers
  }
  deriving (Show)

mkCpu i = CPU i 0 (V.replicate 6 0)


answer1, answer2 :: IO ()
answer1 = do
  (i, instructions) <- getData
  let cpu = mkCpu i
  let states = unfoldr (execCPU' instructions) cpu
  print $ regs (last states) ! 0

answer2 = do
  (i, instructions) <- getData
  let cpu = mkCpu i
  let cpu' = cpu {regs = regs cpu // [(0,1)]}
  let states = unfoldr (execCPU' instructions) cpu'
  print $ regs (last states) ! 0


getVal x st = Just x

getReg x st = st !? fromIntegral x

binOp
  :: (Word32 -> Word32 -> Word32)
  -> (Word32 -> Registers -> Maybe Word32)
  -> (Word32 -> Registers -> Maybe Word32)
  -> Word32
  -> Word32
  -> Word32
  -> Registers
  -> Maybe Registers
binOp op getS0 getS1 s0 s1 target st = do
  a <- getS0 s0 st
  b <- getS1 s1 st
  pure $ st // [(fromIntegral target, op a b)]


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

exec :: Ins -> Word32 -> Word32 -> Word32 -> Registers -> Maybe Registers
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


execCPU :: V.Vector Instruction -> CPU -> Maybe CPU
execCPU instructions cpu = do
  let i = ip cpu
  let ipv = ipVal cpu
  (Instruction ins (a, b, c)) <- instructions !? ipv
  let r0 = regs cpu // [(fromIntegral i, fromIntegral ipv)]
  r1 <- exec ins a b c r0
  ipv' <- fromIntegral . (+1) <$> r1 !? fromIntegral i
  pure $ CPU i ipv' r1

execCPU' a b = fmap (\x -> (x,x)) (execCPU a b)

-- instructions = [minBound..maxBound]

-- data Sample = Sample
--   { beforeS :: CPU
--   , afterS :: CPU
--   , instruction :: (Word32, Word32, Word32, Word32)
--   }
--   deriving (Show)
--
-- -- opcode, a, b, c
-- type Instruction = (Word32, Word32, Word32, Word32)

data Instruction = Instruction Ins (Word32, Word32, Word32)
  deriving (Show)

getData :: IO (Int, V.Vector Instruction)
getData = do
  raw <- Tx.IO.readFile "data/2018/day19.txt"
  case parse parseData "day19" raw of
    Left err -> error $ show err
    Right x -> pure x

parseData :: Parser (Int, V.Vector Instruction)
parseData = (,)
  <$> parseIp <* char '\n'
  <*> parseInstructions

parseIp = string "#ip " *> decimal

parseInstructions = V.fromList <$> U.parseLines parseInstruction

parseInstruction = do
  c <- parseInsCode
  char ' '
  a <- decimal
  char ' '
  b <- decimal
  char ' '
  d <- decimal
  pure $ Instruction c (a, b, d)

parseInsCode :: Parser Ins
parseInsCode =
      try (string "addr" $> Addr)
  <|> try (string "addi" $> Addi)
  <|> try (string "mulr" $> Mulr)
  <|> try (string "muli" $> Muli)
  <|> try (string "banr" $> Banr)
  <|> try (string "bani" $> Bani)
  <|> try (string "borr" $> Borr)
  <|> try (string "bori" $> Bori)
  <|> try (string "setr" $> Setr)
  <|> try (string "seti" $> Seti)
  <|> try (string "gtir" $> Gtir)
  <|> try (string "gtri" $> Gtri)
  <|> try (string "gtrr" $> Gtrr)
  <|> try (string "eqir" $> Eqir)
  <|> try (string "eqri" $> Eqri)
  <|> try (string "eqrr" $> Eqrr)
  <|> error "cannot parse instruction"


test :: IO ()
test = do
  let r = 0
  let is = V.fromList
        [ Instruction Seti (5, 0, 1)
        , Instruction Seti (6, 0, 2)
        , Instruction Addi (0, 1, 0)
        , Instruction Addr (1, 2, 3)
        , Instruction Setr (1, 0, 0)
        , Instruction Seti (8, 0, 4)
        , Instruction Seti (9, 0, 5)
        ]
  let cpu = mkCpu r
  let states = unfoldr (execCPU' is) cpu
  mapM_ print $ take 10 states
  print $ regs (last states) ! 0
