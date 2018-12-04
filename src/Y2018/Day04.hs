{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Y2018.Day04 (answer1, answer2) where

import Data.Foldable
import Control.Monad.Loops as Loops

import GHC.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import           Data.Vector                    ( (//) )
import qualified Data.Vector                   as V
import           Data.Void
import           Data.Functor
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO

answer1, answer2 :: IO ()
answer1 = do
  shifts <- getData
  let m@(mostId, mostVals) = maximumBy (compare `on` (V.sum . snd)) (guardStats shifts)
  print $ mostId * V.maxIndex mostVals

answer2 = do
  shifts <- getData
  let m@(mostId, mostVals) = maximumBy (compare `on` (V.maximum . snd)) (guardStats shifts)
  print $ mostId * V.maxIndex mostVals

data SleepStatus = Sleep | Awake deriving Show
data Date = Date
  { dY :: Int
  , dM :: Int
  , dD :: Int
  , dH :: Int
  , dMin :: Int
  } deriving Show

data Shift = Shift
  { sStart :: Date
  , sGuardId :: Int
  , sEvents :: [(Date, SleepStatus)]
  } deriving (Show)


-- | gives for each guard, a vector of the number of time asleep at minute m
guardStats :: [Shift] -> [(Int, V.Vector Int)]
guardStats shifts =
  let f m s = Map.insertWith (V.zipWith (+)) (sGuardId s) (timeAsleep s) m
      allGuards = Map.toList $ foldl' f Map.empty shifts
  in allGuards

-- | minutes when the guard was asleep for the given shift?
timeAsleep :: Shift -> V.Vector Int
timeAsleep shifts =
  let updates = map (, 1) $ concat $ snd $ foldl' f ((0, Awake), []) $ sEvents shifts
      f ((prevMin, prevStatus), n) ev@(d, status) = case status of
        Sleep -> ((dMin d, status), n)
        Awake -> ((dMin d, status), [prevMin..dMin d-1] : n)
  in V.replicate 60 0 // updates


type Parser = Parsec Void Tx.Text

getData :: IO [Shift]
getData = do
  -- input file sorted beforehand
  raw <- Tx.IO.readFile "data/2018/day04.txt"
  case parse dataParser "day04" raw of
    Left err -> error $ show err
    Right x -> pure x


dataParser :: Parser [Shift]
dataParser = do
  lines <- parseRawLine `untilM` isEOF
  pure $ foldl' foldShifts [] lines

type RawLine = (Either (Date, Int) (Date, SleepStatus))

foldShifts :: [Shift] -> RawLine -> [Shift]
foldShifts acc (Left (d, i)) = Shift d i [] : acc
foldShifts [] (Right _) = error "guard with no shift?"
foldShifts (s:acc) (Right ev) = addEvent ev s : acc
  where
    -- highly non optimal but shouldn't matter
    addEvent e s = s {sEvents = sEvents s <> [e]}

parseRawLine :: Parser RawLine
parseRawLine = try (Left <$> parseGuardLine) <|> Right <$> parseStatusLine

parseShift :: Parser Shift
parseShift = do
  (d, g) <- parseGuardLine
  Shift d g <$> parseStatuses


parseDate :: Parser Date
parseDate = Date
  <$ char '['
  <*> decimal
  <* char '-'
  <*> decimal
  <* char '-'
  <*> decimal
  <* char ' '
  <*> decimal
  <* char ':'
  <*> decimal
  <* char ']'

parseGuardLine :: Parser (Date, Int)
parseGuardLine = (,)
  <$> parseDate
  <* char ' '
  <*> parseGuard
  <* char '\n'

parseGuard :: Parser Int
parseGuard = string "Guard #" *> decimal <* takeWhileP Nothing (/= '\n')

parseStatuses :: Parser [(Date, SleepStatus)]
parseStatuses = loop []
  where
    loop acc = do
      r <- try (isEOF *> fail "eof") <|> try (parseGuardLine $> Nothing) <|> (Just <$> parseStatusLine)
      case r of
        Nothing -> pure (reverse acc)
        Just s -> loop (s : acc)

parseStatusLine :: Parser (Date, SleepStatus)
parseStatusLine = (,)
  <$> parseDate
  <* char ' '
  <*> parseStatus
  <* char '\n'

parseStatus :: Parser SleepStatus
parseStatus
  =   string "falls asleep" $> Sleep
  <|> string "wakes up" $> Awake


isEOF :: Parser Bool
isEOF = (try eof $> True) <|> pure False


testShift1 = Shift
  (Date 1518 11 1 0 0)
  10
  [ (Date 1518 11 1 0 5, Sleep)
  , (Date 1518 11 1 0 25, Awake)
  , (Date 1518 11 1 0 30, Sleep)
  , (Date 1518 11 1 0 55, Awake)
  ]
