{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Y2018.Day21 (answer1, answer2) where

import           GHC.Word
import           Data.Void
import           Data.Bits
import           Data.Functor
import           Data.List
import qualified Data.Vector                   as V
import           Data.Vector                    ( (!), (//), (!?) )
import qualified Data.Set                      as S

import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Utils.Parser                  as U

import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

type Parser = Parsec Void Tx.Text

type Registers = V.Vector Word
data CPU = CPU
  { ip :: Int
  , ipVal :: Int
  , regs :: Registers
  }
  deriving (Show)

data Instruction = Instruction Ins (Word, Word, Word)
  deriving (Show)


mkCpu i r0 =
  let regs = V.replicate 6 0 // [(0, r0)]
   in CPU i 0 regs

answer1, answer2 :: IO ()
answer1 = do
  (r, is) <- getData
  let states = computeAllStates r is

  -- there is only one instruction which checks the value of r0
  let res = head $ map (\s -> regs s ! 3) $ filter ((== 28) . ipVal) states
  print res

answer2 = do
  (r, is) <- getData
  let states = computeAllStates r is
  let r3Vals = map (\s -> regs s ! 3) $ filter ((== 28) . ipVal) states
  let allBeforeLoop = takeUntilRepeat r3Vals

  print $ last allBeforeLoop

computeAllStates r is = unfoldr (execCPU' $ optimise is) (mkCpu r 0)

-- There is a small loop which makes a really inneficient division.
optimise :: V.Vector Instruction -> V.Vector Instruction
optimise is =
  let f = \case
        (Instruction Addi (4,1,4)) -> Instruction Divi (1, 256, 4)
        i -> i
   in V.map f is

takeUntilRepeat :: (Eq a, Ord a) => [a] -> [a]
takeUntilRepeat = go mempty []
  where
    go _ acc [] = reverse acc
    go seen acc (x:xs) = if S.member x seen
      then reverse acc
      else go (S.insert x seen) (x:acc) xs

getVal x st = Just x

getReg x st = st !? fromIntegral x

binOp
  :: (Word -> Word -> Word)
  -> (Word -> Registers -> Maybe Word)
  -> (Word -> Registers -> Maybe Word)
  -> Word
  -> Word
  -> Word
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
  | Divi
  deriving (Show, Eq, Bounded, Enum, Ord)

exec :: Ins -> Word -> Word -> Word -> Registers -> Maybe Registers
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
  Divi -> binOp div getReg getVal


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

getData :: IO (Int, V.Vector Instruction)
getData = do
  raw <- Tx.IO.readFile "data/2018/day21.txt"
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
  <|> try (string "divi" $> Divi)
  <|> error "cannot parse instruction"


prettyCPU :: CPU -> String
prettyCPU cpu
  = show (ipVal cpu)
  <> "\t|\t"
  <> intercalate "\t" (V.toList (fmap show (regs cpu)))
