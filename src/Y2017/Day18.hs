{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2017.Day18 (answer1, answer2) where

import Data.Functor
import Data.Maybe
import Control.Monad
import Control.Monad.State.Lazy as S
import Control.Monad.Loops
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.HashMap.Strict as Map

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TMChan as Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Exception as Exc

answer1, answer2 :: IO Int
answer1 = do
    is <- parseInput
    let initialState = (0, V.replicate 5 0, Nothing)

    let haltCond = do
            (i, regs, f) <- get
            if i < 0 || i >= V.length is
                then pure True
                else do
                    let ins = is V.! i
                    case ins of
                        (Recover _) -> pure True
                        _           -> pure False

    let prog      = execInstructions is `untilM_` haltCond
    let (_, _, f) = execState prog initialState
    case f of
        Nothing   -> error "program halted without receiving anything"
        Just freq -> pure freq

answer2 = do
    is  <- parseInput
    ch0 <- newTMChanIO
    ch1 <- newTMChanIO
    c0  <- newTVarIO 0
    c1  <- newTVarIO 0
    let state0 = (0, V.fromList [0, 0, 0, 0, 0], Nothing)
    let state1 = (0, V.fromList [0, 0, 0, 1, 0], Nothing)
    let prg0   = runAsync c0 ch0 ch1 is state0
    let prg1   = runAsync c1 ch1 ch0 is state1
    (res0, res1) <-
        Async.concurrently prg0 prg1
            `Exc.catch` \Exc.BlockedIndefinitelyOnMVar ->
                            (,) <$> readTVarIO c0 <*> readTVarIO c1
    print (res0, res1)
    pure res1


type Registers = V.Vector Int

-- instruction pointer, registers, last emitted sound
type CpuState = (Int, Registers, Maybe Int)

data Reg = Reg Int | Val Int deriving Show

data Instruction
    = Send Int
  | Set Int Reg
  | Add Int Reg
  | Mul Int Reg
  | Mod Int Reg
  | Jump Reg Reg
  | Recover Int
    deriving Show

data Status = Running CpuState | Halted deriving (Show, Eq)

runAsync
    :: TVar Int
    -> TMChan Int
    -> TMChan Int
    -> V.Vector Instruction
    -> CpuState
    -> IO Int
runAsync counter sendChan rcvChan is state@(i, _, _) =
    if i < 0 || i >= V.length is
        then atomically $ closeTMChan sendChan *> readTVar counter
        else do
            let ins = is V.! i
            status <- execInstructionAsync counter sendChan rcvChan ins state
            case status of
                Halted -> atomically $ closeTMChan sendChan *> readTVar counter
                Running s' -> runAsync counter sendChan rcvChan is s'


execInstructions :: V.Vector Instruction -> S.State CpuState ()
execInstructions instructions = do
    (i, regs, _) <- get
    if i < 0 || i > V.length instructions
        then pure ()
        else do
            let ins = instructions V.! i
            S.modify' (execInstruction' ins)

execInstruction' :: Instruction -> CpuState -> CpuState
execInstruction' (Send x) (i, regs, _) = (i + 1, regs, Just $ regs V.! x)
execInstruction' (Set x r) (i, regs, f) =
    (i + 1, regs V.// [(x, getVal regs r)], f)
execInstruction' (Add x r) (i, regs, f) =
    (i + 1, regs V.// [(x, regs V.! x + getVal regs r)], f)
execInstruction' (Mul x r) (i, regs, f) =
    (i + 1, regs V.// [(x, regs V.! x * getVal regs r)], f)
execInstruction' (Mod x r) (i, regs, f) =
    (i + 1, regs V.// [(x, regs V.! x `mod` getVal regs r)], f)
execInstruction' (Jump x r) (i, regs, f) =
    if getVal regs x > 0 then (i + getVal regs r, regs, f) else (i + 1, regs, f)
execInstruction' (Recover x) (i, regs, f) = (i + 1, regs, f)


execInstructionAsync
    :: TVar Int
    -> TMChan Int
    -> TMChan Int
    -> Instruction
    -> CpuState
    -> IO Status
execInstructionAsync c sendChan _ (Send x) (i, regs, f) = atomically $ do
    writeTMChan sendChan (regs V.! x)
    modifyTVar' c        (+1)
    pure $ Running (i + 1, regs, f)
execInstructionAsync c _ rcvChan (Recover x) (i, regs, f) = do
    mbVal <- atomically $ readTMChan rcvChan
    pure $ case mbVal of
        Nothing -> Halted
        Just v  -> Running (i + 1, regs V.// [(x, v)], f)
execInstructionAsync _ _ _ ins state = do
    let s' = execInstruction' ins state
    pure $ Running s'


getVal :: Registers -> Reg -> Int
getVal _    (Val i) = i
getVal regs (Reg i) = regs V.! i

parseInput :: IO (V.Vector Instruction)
parseInput = do
    raw <- readFile "./data/2017/day18.txt"
    case parse parseInstructions "day 18" raw of
        Right x   -> pure $ V.fromList x
        Left  err -> error $ show err

parseInstructions = parseInstruction `sepEndBy` newline

parseInstruction =
    ((string "snd" $> Send) <*> (space *> parseDest))
        <|> (   (string "set" $> Set)
            <*> (space *> parseDest)
            <*> (space *> parseRegister)
            )
        <|> (   (string "add" $> Add)
            <*> (space *> parseDest)
            <*> (space *> parseRegister)
            )
        <|> (   (string "mul" $> Mul)
            <*> (space *> parseDest)
            <*> (space *> parseRegister)
            )
        <|> (   (string "mod" $> Mod)
            <*> (space *> parseDest)
            <*> (space *> parseRegister)
            )
        <|> (   (string "jgz" $> Jump)
            <*> (space *> parseRegister)
            <*> (space *> parseRegister)
            )
        <|> ((string "rcv" $> Recover) <*> (space *> parseDest))

parseRegister = (Reg <$> parseDest) <|> (Val <$> parseDigit)

parseDest :: Parser Int
parseDest =
    (char 'a' $> 0)
        <|> (char 'b' $> 1)
        <|> (char 'i' $> 2)
        <|> (char 'p' $> 3)
        <|> (char 'f' $> 4)

parseDigit :: Parser Int
parseDigit = do
    sign <- optional (char '-')
    d    <- some digitChar
    pure $ case sign of
        Nothing -> read d
        Just _  -> read d * (-1)
