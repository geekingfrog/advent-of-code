module Y2015.Day08 (answer1, answer2) where

import Control.Monad (fmap)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char (chr, digitToInt)
import Control.Exception (throw)

type Parser = Parsec Void String

answer1 :: IO ()
answer1 = do
    strings <- getData
    print $ sum (map length strings) - sum (map countRealChars strings)

answer2 :: IO ()
answer2 = do
    strings <- getData
    print $ sum (map (length . show) strings) - sum (map length strings)

getData :: IO [String]
getData = fmap lines (readFile "./data/08.txt")

countRealChars :: String -> Int
countRealChars line = case runParser parseLine "parseLine" line of
    Left  err    -> throw err
    Right parsed -> length parsed

parseLine = between (char '"') (char '"') (many maybeEscapedChar)

maybeEscapedChar =
    try hexChar
        <|> try (string "\\\\" >> return '\\')
        <|> try (string "\\\"" >> return '"')
        <|> letterChar

hexChar :: Parser Char
hexChar = do
    char '\\'
    char 'x'
    a <- fmap digitToInt hexDigitChar
    b <- fmap digitToInt hexDigitChar
    return $ chr (16 * a + b)
