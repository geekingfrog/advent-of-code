module Y2015.Day08 (answer1, answer2) where

import Control.Monad (liftM)
import Text.Megaparsec
import Text.Megaparsec.String
import Data.Char (chr, digitToInt)
import Control.Exception (throw)

answer1 :: IO Int
answer1 = do
    strings <- getData
    return $ sum (map length strings) - sum (map countRealChars strings)

answer2 :: IO Int
answer2 = do
    strings <- getData
    return $ sum (map (length . show) strings) - sum (map length strings)

getData :: IO [String]
getData = liftM lines (readFile "./data/08.txt")

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
    a <- liftM digitToInt hexDigitChar
    b <- liftM digitToInt hexDigitChar
    return $ chr (16 * a + b)
