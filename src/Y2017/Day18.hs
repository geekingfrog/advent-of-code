{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Y2017.Day18 (answer1, answer2) where

import Data.Functor
import Data.Maybe
import Data.Void
import Control.Monad
import Control.Monad.State.Lazy as S
import Control.Monad.Loops
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict as Map

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TMChan as Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Exception as Exc
import Control.Monad.IO.Class (liftIO)

import Control.Monad.Reader

type Parser = Parsec Void String

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

    let env0 = PrgEnv
            { prgName         = "prg0"
            , prgSendChan     = ch0
            , prgRcvChan      = ch1
            , prgCounter      = c0
            , prgInstructions = is
            }

    let env1 = PrgEnv
            { prgName         = "prg1"
            , prgSendChan     = ch1
            , prgRcvChan      = ch0
            , prgCounter      = c1
            , prgInstructions = is
            }

    let prg0 = runReaderT (runAsync state0) env0
    let prg1 = runReaderT (runAsync state1) env1
    (res0, res1) <-
        Async.concurrently prg0 prg1
            `Exc.catch` \Exc.BlockedIndefinitelyOnMVar ->
                            (,) <$> readTVarIO c0 <*> readTVarIO c1
    pure res1


type Registers = V.Vector Int
data PrgEnv = PrgEnv
    { prgSendChan :: TMChan Int
    , prgRcvChan :: TMChan Int
    , prgCounter :: TVar Int
    , prgName :: String
    , prgInstructions :: V.Vector Instruction
    }

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

runAsync :: CpuState -> ReaderT PrgEnv IO Int
runAsync state@(i, _, _) = do
    env <- ask
    let is       = prgInstructions env
        sendChan = prgSendChan env
        counter  = prgCounter env
    if i < 0 || i >= V.length is
        then liftIO $ atomically $ closeTMChan sendChan *> readTVar counter
        else do
            let ins = is V.! i
            status <- execInstructionAsync state
            case status of
                Halted ->
                    liftIO
                        $  atomically
                        $  closeTMChan sendChan
                        *> readTVar counter
                Running s' -> runAsync s'


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


execInstructionAsync :: CpuState -> ReaderT PrgEnv IO Status
execInstructionAsync (i, regs, f) = do
    env <- ask
    let ins = prgInstructions env V.! i
    case ins of
        (Send x) -> do
            liftIO $ atomically $ do
                writeTMChan (prgSendChan env) (regs V.! x)
                modifyTVar' (prgCounter env)  (+1)
            pure $ Running (i + 1, regs, f)
        (Recover x) -> do
            mbVal <- liftIO $ atomically $ readTMChan (prgRcvChan env)
            pure $ case mbVal of
                Nothing -> Halted
                Just v  -> Running (i + 1, regs V.// [(x, v)], f)
        instruction -> do
            let s' = execInstruction' ins (i, regs, f)
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
        Just _  -> negate $ read d
