module Day06 (answer1, answer2) where

import Prelude hiding (replicate)
import Control.Monad (liftM)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.String
import Control.Exception (throw, SomeException)
import Data.Vector (Vector, replicate, (//), (!))
import Data.List (foldl')

answer1 :: IO Int
answer1 = do
  instructions <- getInstructions
  let finalGrid = foldl' applyInstruction initialGrid instructions
  return $ countLightOn finalGrid

answer2 :: IO Int
answer2 = do
  instructions <- getInstructions
  let finalGrid = foldl' applyInstruction2 initialBrightGrid instructions
  return $ totalBrightness finalGrid

------------------------------------------------------------
--  Some types
------------------------------------------------------------
type Instruction = (Switch, RectanglePos)
type Pos = (Int, Int)
type RectanglePos = (Pos, Pos)
data Switch = TurnOn | TurnOff | Toggle deriving (Show, Eq)
data LightStatus = On | Off deriving (Show, Eq)
type Grid = Vector LightStatus

type BrightnessStatus = Int
type BrightGrid = Vector BrightnessStatus

------------------------------------------------------------
--  Core logic Day 1
------------------------------------------------------------

limit = 1000

initialGrid :: Grid
initialGrid = replicate (limit*limit) Off

posToIndex :: Pos -> Int
posToIndex (x, y) = x + limit*y

indexToPos :: Int -> Pos
indexToPos i = (i `mod` limit, i `quot` limit)

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction g (TurnOn,  (bl, tr)) = g // genUpdates On bl tr
applyInstruction g (TurnOff, (bl, tr)) = g // genUpdates Off bl tr
applyInstruction g (Toggle,  (bl, tr)) = g // genToggles g bl tr

turnOn grid bl tr = grid // genUpdates On bl tr
turnOff = undefined

genUpdates :: LightStatus -> Pos -> Pos -> [(Int, LightStatus)]
genUpdates light (x0, y0) (x1, y1) = [(posToIndex (x,y), light) | x <- [x0..x1], y <- [y0..y1]]

genToggles :: Grid -> Pos -> Pos -> [(Int, LightStatus)]
genToggles g (x0, y0) (x1, y1)= updates
  where
    updates = [(i, if (g ! i) == On then Off else On) |
                x <- [x0..x1],
                y <- [y0..y1],
                let i = posToIndex (x, y)
              ]


countLightOn :: Grid -> Int
countLightOn = foldl' isOn 0
  where isOn acc On = acc + 1
        isOn acc _  = acc

------------------------------------------------------------
--  Core logic Day 2
------------------------------------------------------------
initialBrightGrid :: BrightGrid
initialBrightGrid = replicate (limit*limit) 0

applyInstruction2 :: BrightGrid -> Instruction -> BrightGrid
applyInstruction2 g (TurnOn,  (bl, tr)) = increaseBy 1    g bl tr
applyInstruction2 g (TurnOff, (bl, tr)) = increaseBy (-1) g bl tr
applyInstruction2 g (Toggle,  (bl, tr)) = increaseBy 2    g bl tr

increaseBy :: Int -> BrightGrid -> Pos -> Pos -> BrightGrid
increaseBy n g (x0, y0) (x1, y1) = g // updates
  where updates = [ (i, modify (g ! i)) |
                    x <- [x0..x1],
                    y <- [y0..y1],
                    let i = posToIndex (x, y)
                  ]
        modify k = max 0 (k+n)

totalBrightness :: BrightGrid -> Int
totalBrightness = foldl' (+) 0

------------------------------------------------------------
--  Parsing
------------------------------------------------------------

getInstructions :: IO [Instruction]
getInstructions = do
  content <- readFile "./data/06.txt"
  case parse instructionsParser "" content of
    Left err -> throw err
    Right instructions -> return instructions

instructionsParser :: Parser [Instruction]
instructionsParser = sepEndBy instructionLine newline

-- assume start < end
instructionLine = do
  s <- switch
  space
  start <- position
  string " through "
  end <- position
  return (s, (start, end))

switch = try (string "turn on"  >> return TurnOn)
     <|> try (string "turn off" >> return TurnOff)
     <|> (string "toggle"   >> return Toggle)

position = do
  p1 <- read <$> some digitChar
  char ','
  p2 <- read <$> some digitChar
  return (p1, p2)
