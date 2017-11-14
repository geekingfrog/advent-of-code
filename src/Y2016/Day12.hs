{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day12 (answer1, answer2) where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.HashMap.Strict as M
import Data.Vector as V (foldl')
import Data.Aeson
import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)

answer1 :: IO Int
answer1 = liftM sumNumbers getData

answer2 :: IO Int
answer2 = liftM (sumNumbers . removeRed) getData

sumNumbers :: Value -> Int
sumNumbers (Object o) = M.foldl' (\acc v -> acc + sumNumbers v) 0 o
sumNumbers (Array  a) = V.foldl' (\acc v -> acc + sumNumbers v) 0 a
sumNumbers (Number n) = fromMaybe 0 (toBoundedInteger n)
sumNumbers _          = 0

getData :: IO Value
getData = do
    raw <- readFile "./data/12.json"
    case eitherDecode' raw of
        Left  err -> error err
        Right val -> return val

removeRed :: Value -> Value
removeRed (Object o) =
    if hasRed o then Object M.empty else Object (M.map removeRed o)
removeRed (Array a) = Array (fmap removeRed a)
removeRed val       = val

hasRed o = elem (String "red") $ M.elems o
