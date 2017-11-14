module Day16 (answer1, answer2) where

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Exception
import Data.Function (on)
import Control.Monad (liftM)

answer1 :: IO Int
answer1 = liftM (fst . head . filter checkAunt1) getData

answer2 :: IO Int
answer2 = liftM (fst . head . filter checkAunt2) getData

type Name = Int
type Attr = (String, Int)

knownInfo :: [Attr]
knownInfo =
    [ ("children"   , 3)
    , ("cats"       , 7)
    , ("samoyeds"   , 2)
    , ("pomeranians", 3)
    , ("akitas"     , 0)
    , ("vizslas"    , 0)
    , ("goldfish"   , 5)
    , ("trees"      , 3)
    , ("cars"       , 2)
    , ("perfumes"   , 1)
    ]

checkAunt1 :: (Name, [Attr]) -> Bool
checkAunt1 (_, attrs) = all (checkAttr (\_ a b -> a == b) attrs) knownInfo

checkAunt2 (_, attrs) = all (checkAttr rangeComparator attrs) knownInfo
  where
    rangeComparator "cats"        source target = source > target
    rangeComparator "trees"       source target = source > target
    rangeComparator "pomeranians" source target = source < target
    rangeComparator "goldfish"    source target = source < target
    rangeComparator _             source target = source == target


checkAttr :: (String -> Int -> Int -> Bool) -> [Attr] -> Attr -> Bool
checkAttr _          []            _                  = True
checkAttr comparator ((x, y):rest) attr@(target, val) = if x == target
    then comparator target y val
    else checkAttr comparator rest attr


getData :: IO [(Name, [Attr])]
getData = do
    content <- readFile "./data/16.txt"
    case parse auntsParser "" content of
        Left  err -> throw err
        Right as  -> return as

auntsParser :: Parser [(Name, [Attr])]
auntsParser = sepEndBy auntParser newline

auntParser =
    (,)
        <$> (string "Sue " *> (read <$> some digitChar) <* string ": ")
        <*> attrs

attrs = sepEndBy attr (string ", ")
attr = (,) <$> (some letterChar <* string ": ") <*> (read <$> some digitChar)
