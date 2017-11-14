module Day23 (answer1, answer2) where

import Control.Exception (throw)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Vector as V
import Data.List (unfoldr)
import Control.Monad (liftM)

answer1 :: IO Int
answer1 = do
  prog <- getData
  let states = unfoldr (run prog) (0,0,0)
  return . second $ last states

answer2 :: IO Int
answer2 = do
  prog <- getData
  let states = unfoldr (run prog) (1,0,0)
  return . second $ last states

data Reg = A | B deriving (Show, Eq)
data Instruction = Half Reg
                 | Triple Reg
                 | Incr Reg
                 | Jump Int
                 | JumpEven Reg Int
                 | JumpOne Reg Int
                 deriving (Show, Eq)
type ProgramState = (Int, Int, Int)
type Program = V.Vector Instruction

run :: Program -> ProgramState -> Maybe (ProgramState, ProgramState)
run program s@(_, _, i) = case program V.!? i of
  Nothing -> Nothing
  Just instruction -> Just (nextState, nextState)
    where nextState = execute s instruction

execute :: ProgramState -> Instruction -> ProgramState
execute (a, b, i) (Half A) = (a `quot` 2, b, i+1)
execute (a, b, i) (Half B) = (a, b `quot` 2, i+1)
execute (a, b, i) (Triple A) = (a*3, b, i+1)
execute (a, b, i) (Triple B) = (a, b*3, i+1)
execute (a, b, i) (Incr A) = (a+1, b, i+1)
execute (a, b, i) (Incr B) = (a, b+1, i+1)
execute (a, b, i) (Jump j) = (a, b, i + j)
execute s@(a, b, i) (JumpEven r o) = if (r == A && even a) || (r == B && even b)
                                     then execute s (Jump o)
                                     else (a, b, i+1)
execute s@(a, b, i) (JumpOne r o) = if (r == A && a == 1) || (r == B && b == 1)
                                    then execute s (Jump o)
                                    else (a, b, i+1)

second (_, a, _) = a

getData :: IO (V.Vector Instruction)
getData = do
  content <- readFile "./data/23.txt"
  case parse instructionsParser "" content of
    Left err -> throw err
    Right is -> return is

instructionsParser = V.fromList <$> sepEndBy instructionParser newline

instructionParser = try (Half <$> (string "hlf " >> registerParser))
                <|> try (Triple <$> (string "tpl " >> registerParser))
                <|> try (Incr <$> (string "inc " >> registerParser))
                <|> try (Jump <$> (string "jmp " >> offsetParser))
                <|> try (JumpEven <$> (string "jie " >> registerParser) <*> (string ", " >> offsetParser))
                <|> (JumpOne <$> (string "jio " >> registerParser) <*> (string ", " >> offsetParser))

registerParser = (char 'a' >> return A) <|> (char 'b' >> return B)

offsetParser :: Parser Int
offsetParser = read <$> (char '+' >> some digitChar)
           <|> ((*(-1)) . read) <$> (char '-' >> some digitChar)

testP = V.fromList [
  Incr A,
  JumpOne A 2,
  Triple A,
  Incr A
  ]
