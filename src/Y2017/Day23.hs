{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2017.Day23
  ( answer1
  , answer2
  ) where

import Data.Char
import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict as S
import Control.Monad.Loops
import Data.Vector ((!))
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.HashMap.Strict as Map

import Data.Numbers.Primes

answer1, answer2 :: IO Int
answer1 = do
    instructions <- parseInput "./data/2017/day23.txt"
    let regs = V.replicate (ord 'h' - ord 'a' + 1) 0
    let initialState = (0, regs)
    let prog = unfoldM (runStep instructions)
    let executed = evalState prog initialState
    pure $ length $ filter isMul executed

answer2 = do
    -- after a long time trying to see what the program
    -- was doing, here's the desugared form
    let b0 = 65 * 100 + 100000
    let c = b0 + 17000
    let ns = [b0,b0 + 17 .. c]
    pure $ length $ filter (not . isPrime) ns

type Registers = V.Vector Int

-- instruction pointer, registers
type CpuState = (Int, Registers)

data Reg
    = Reg Int
    | Val Int
     deriving (Show)

data Instruction
    = Set Int
          Reg
    | Sub Int
          Reg
    | Mul Int
          Reg
    | Jump Reg
           Reg
     deriving (Show)

isMul (Mul _ _) = True
isMul _ = False

runStep :: V.Vector Instruction -> S.State CpuState (Maybe Instruction)
runStep instructions = do
    i <- getInstruction instructions
    traverse execInstruction i
    pure i

getInstruction :: V.Vector Instruction -> S.State CpuState (Maybe Instruction)
getInstruction is = do
    (i, _) <- get
    if i < 0 || i >= V.length is
        then pure Nothing
        else pure $ Just (is ! i)

execInstruction :: Instruction -> S.State CpuState ()
execInstruction (Set x y) = do
    (i, regs) <- get
    let regs' = regs V.// [(x, getVal regs y)]
    put (i + 1, regs')
execInstruction (Sub x y) = do
    (i, regs) <- get
    let regs' = regs V.// [(x, regs V.! x - getVal regs y)]
    put (i + 1, regs')
execInstruction (Mul x y) = do
    (i, regs) <- get
    let regs' = regs V.// [(x, regs V.! x * getVal regs y)]
    put (i + 1, regs')
execInstruction (Jump x y) = do
    (i, regs) <- get
    let v = getVal regs x
    let offset = getVal regs y
    let i' =
            if v == 0
                then i + 1
                else i + offset
    put (i', regs)

getVal :: Registers -> Reg -> Int
getVal _ (Val i) = i
getVal regs (Reg i) = regs V.! i

parseInput :: FilePath -> IO (V.Vector Instruction)
parseInput path = do
    raw <- readFile path
    case parse parseInstructions "day 23" raw of
        Right x -> pure $ V.fromList x
        Left err -> error $ show err

parseInstructions = parseInstruction `endBy` newline <* eof

parseInstruction =
    ((string "set" $> Set) <*> (space *> parseDest) <*> (space *> parseRegister)) <|>
    ((string "sub" $> Sub) <*> (space *> parseDest) <*> (space *> parseRegister)) <|>
    ((string "mul" $> Mul) <*> (space *> parseDest) <*> (space *> parseRegister)) <|>
    ((string "jnz" $> Jump) <*> (space *> parseRegister) <*>
     (space *> parseRegister))

parseRegister = (Reg <$> parseDest) <|> (Val <$> parseDigit)

parseDest :: Parser Int
parseDest = do
    c <- letterChar
    pure $ ord c - 97

parseDigit :: Parser Int
parseDigit = do
    sign <- optional (char '-')
    d <- some digitChar
    pure $
        case sign of
            Nothing -> read d
            Just _ -> negate $ read d
