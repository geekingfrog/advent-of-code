module Y2015.Day07 (answer1, answer2) where

import Data.Word (Word16)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Control.Monad (liftM)
import Control.Monad.State

import Text.Megaparsec hiding (State, Label)
import Text.Megaparsec.String
import Control.Exception (throw)

answer1 :: IO Int
answer1 = do
    instructions <- getInstructions
    let env = M.fromList instructions
    return . fromIntegral $ evalState (eval $ Label "a") env

answer2 :: IO Int
answer2 = do
    instructions <- getInstructions
    let env                    = M.fromList instructions
    -- get answer from first puzzle as well as the final state
    let (result1, finalState1) = runState (eval $ Label "a") env
    -- override b with the result of wire a
    let env2                   = M.insert "b" (Val result1) env
    let result2                = evalState (eval $ Label "a") env2
    return $ fromIntegral result2

------------------------------------------------------------
--  Some types
------------------------------------------------------------

data Expr = Val Word16
          | Label String
          | Not Expr
          | And Expr Expr
          | Or  Expr Expr
          | LShift Expr Int
          | RShift Expr Int
          deriving (Show, Eq)

type Env = M.HashMap String Expr
type Instruction = (String, Expr)  -- output <- Expr


------------------------------------------------------------
--  Core logic
------------------------------------------------------------
eval :: Expr -> State Env Word16
eval (Val   a) = return a
eval (Label a) = do
    env <- get
    case M.lookup a env of
        Nothing       -> error $ "Cannot find label " ++ show a
        -- Just newlabel -> eval newlabel >>= put . M.insert a
        Just newLabel -> do
            result <- eval newLabel
            modify $ M.insert a (Val result)
            return result

eval (Not a     ) = liftM complement (eval a)
eval (And    a b) = liftM2 (.&.) (eval a) (eval b)
eval (Or     a b) = liftM2 (.|.) (eval a) (eval b)
eval (LShift a s) = eval a >>= \res -> return $ shiftL res s
eval (RShift a s) = eval a >>= \res -> return $ shiftR res s

------------------------------------------------------------
--  Parsing
------------------------------------------------------------

getInstructions :: IO [Instruction]
getInstructions = do
    content <- readFile "./data/07.txt"
    case parse instructionsParser "" content of
        Left  err          -> throw err
        Right instructions -> return instructions

instructionsParser :: Parser [Instruction]
instructionsParser = sepEndBy instructionLine newline

instructionLine =
    try initLine <|> try notLine <|> try binaryInstructionLine <|> bitShiftLine

initLine = do
    l      <- parseLabel
    output <- parseOutput
    return (output, l)

notLine = do
    string "NOT "
    input  <- parseLabel
    output <- parseOutput
    return (output, Not input)

binaryInstructionLine = do
    a <- parseLabel
    space
    rawI <- try (string "AND") <|> string "OR"
    space
    b      <- parseLabel
    output <- parseOutput
    return (output, instruction rawI a b)
  where
    instruction "AND" = And
    instruction _     = Or

bitShiftLine = do
    a <- parseLabel
    space
    rawI <- try (string "RSHIFT") <|> string "LSHIFT"
    space
    b      <- read <$> many digitChar
    output <- parseOutput
    return (output, instruction rawI a b)
  where
    instruction "LSHIFT" = LShift
    instruction _        = RShift

parseLabel =
    (liftM Label $ some letterChar) <|> (liftM (Val . read) $ some digitChar)
parseOutput = string " -> " >> many lowerChar
