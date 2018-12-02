module Y2017.Day09 (answer1, answer2) where


import Data.Functor
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

answer1, answer2 :: IO ()
answer1 = parseInput >>= print . streamSize
answer2 = parseInput >>= print . garbageSize


streamSize = go 1
  where go n (Garbage _) = 0
        go n (Group gs) = n + sum (fmap (go (n+1)) gs)


garbageSize (Garbage t) = T.length t
garbageSize (Group gs) = sum $ fmap garbageSize gs


data Stuff = Group [Stuff] | Garbage Text deriving Show

parseInput = parseStream <$> T.readFile "data/2017/day09.txt"


parseStream :: Text -> Stuff
parseStream s = case parse parseGroup "stream" s of
    Left  err -> error (show err)
    Right x   -> x

parseGroup = Group <$> between (char '{') (char '}') (parseStuff `sepBy` char ',')

parseStuff :: Parser Stuff
parseStuff = parseGroup <|> parseGarbage

parseGarbage = Garbage <$> between (char '<') (char '>') garbageContent


-- don't parse cancelled characters
garbageContent :: Parser Text
garbageContent = T.concat <$> many
    (   try (char '!' *> (T.singleton <$> asciiChar) $> T.pack "")
    <|> (T.singleton <$> satisfy (/= '>'))
    )
