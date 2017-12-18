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

answer1, answer2 :: IO Int
answer1 = do
    is <- parseInput
    let initialState = (0, Map.empty, 0)
    let finalState@(i, regs, s) = execState (iterateWhile isNothing (execute is)) initialState
    pure s
answer2 = error "wip2"


third (_, _, a) = a


-- instruction pointer, registers state, last sound played
type CpuState = (Int, Map.HashMap Char Int, Int)

data Reg = Sym Char | Val Int deriving Show
data Instruction
    = Send Reg
    | Set Reg Reg
    | Add Reg Reg
    | Mul Reg Reg
    | Mod Reg Reg
    | Recover Reg
    | Jump Reg Reg
    deriving Show



execute :: V.Vector Instruction -> S.State CpuState (Maybe Int)
execute is = do
    state@(i, regs, s) <- get
    if i < 0 || i >= V.length is
       then pure Nothing
       else do
           let instruction = is V.! i
           modify' (executeInstruction instruction)
           case instruction of
             Recover r -> (Just . third) <$> get
             _ -> pure Nothing



executeInstruction :: Instruction -> CpuState -> CpuState
executeInstruction (Send r) (i, regs, _) = (i + 1, regs, registerValue regs r)
-- TODO improve datatype to make that impossible
executeInstruction (Set (Sym c) r) (i, regs, s) =
    (i + 1, Map.insert c (registerValue regs r) regs, s)
executeInstruction (Add r1@(Sym c) r2) (i, regs, s) =
    ( i + 1
    , Map.insert c (registerValue regs r1 + registerValue regs r2) regs
    , s
    )
executeInstruction (Mul r1@(Sym c) r2) (i, regs, s) =
    ( i + 1
    , Map.insert c (registerValue regs r1 * registerValue regs r2) regs
    , s
    )
executeInstruction (Mod r1@(Sym c) r2) (i, regs, s) =
    ( i + 1
    , Map.insert c (registerValue regs r1 `mod` registerValue regs r2) regs
    , s
    )
executeInstruction (Recover r@(Sym c)) (i, regs, s) =
    let freq = registerValue regs r
    in  if freq == 0 then (i + 1, regs, s) else (i + 1, regs, freq)

executeInstruction (Jump r@(Sym c) r2) (i, regs, s) =
    let v = registerValue regs r
    in  if v == 0 then (i + 1, regs, s) else (i + v, regs, s)
executeInstruction i _ = error $ "cannot execute " ++ show i



registerValue :: Map.HashMap Char Int -> Reg -> Int
registerValue regs (Sym r) = Map.lookupDefault 0 r regs
registerValue _ (Val v) = v


data ParseToken
    = SendP
    | SetP
    | AddP
    | MulP
    | ModP
    | RecoverP
    | JumpP
    deriving Show


parseInput :: IO (V.Vector Instruction)
parseInput = do
    raw <- readFile "./data/2017/day18.txt"
    case parse parseInstructions "day 18" raw of
        Right x   -> pure $ V.fromList x
        Left  err -> error $ show err

parseInstructions = parseInstruction `sepEndBy` newline

parseInstruction = do
    i <- parseCommand
    space
    r1 <- parseRegister
    case i of
      SendP -> pure $ Send r1
      RecoverP -> pure $ Recover r1
      x -> do
          space
          r2 <- parseRegister
          let constructor = case x of
                AddP -> Add
                ModP -> Mod
                SetP -> Set
                MulP -> Mul
                JumpP -> Jump
                _ -> error "wtf?"
          pure $ constructor r1 r2

parseCommand :: Parser ParseToken
parseCommand =
    try (string "set" $> SetP)
        <|> try (string "snd" $> SendP)
        <|> try (string "add" $> AddP)
        <|> try (string "mul" $> MulP)
        <|> try (string "mod" $> ModP)
        <|> try (string "rcv" $> RecoverP)
        <|> (string "jgz" $> JumpP)

parseRegister = (Sym <$> letterChar) <|> (Val <$> parseDigit)
parseDigit = do
    sign <- optional (char '-')
    d    <- some digitChar
    pure $ case sign of
        Nothing -> read d
        Just _  -> read d * (-1)


test = V.fromList
    [ Set (Sym 'a') (Val 1)
    , Add (Sym 'a') (Val 2)
    , Mul (Sym 'a') (Sym 'a')
    , Mod (Sym 'a') (Val 5)
    , Send (Sym 'a')
    , Set (Sym 'a') (Val 0)
    , Recover (Sym 'a')
    , Jump (Sym 'a') (Val (-1))
    , Jump (Sym 'a') (Val (-2))
    ]
