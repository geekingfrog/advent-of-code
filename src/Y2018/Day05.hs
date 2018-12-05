module Y2018.Day05
  ( answer1
  , answer2
  )
where

import           Data.Void
import           Data.Functor
import           Data.Foldable

import           Data.Char                     as C
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx.IO
import qualified Data.Vector                   as V

answer1, answer2 :: IO ()
answer1 = Tx.length . cycleReact <$> getData >>= print
answer2 = do
  p <- getData
  let allPolysLength = map (Tx.length . cycleReact . removeUnit p) ['a' .. 'z']
  print $ minimum allPolysLength

removeUnit :: Tx.Text -> Char -> Tx.Text
removeUnit t c = Tx.filter ((/= c) . C.toLower) t

cycleReact :: Tx.Text -> Tx.Text
cycleReact tx =
  let allPoly = iterate reactAll tx
      zipped  = zip allPoly (tail allPoly)
  in  case find (uncurry (==)) zipped of
        Just (p, _) -> p
        Nothing     -> error "wut?"

reactAll :: Tx.Text -> Tx.Text
reactAll tx =
  let (mbC, s) = Tx.foldr react (Nothing, "") tx
      finalS   = maybe s (: s) mbC
  in  Tx.pack finalS

react :: Char -> (Maybe Char, String) -> (Maybe Char, String)
react c  (Nothing, s) = (Just c, s)
react c2 (Just c1, s) = if C.toLower c1 == C.toLower c2 && c1 /= c2
  then (Nothing, s)
  else (Just c2, c1 : s)


getData :: IO Tx.Text
getData = Tx.filter (/= '\n') <$> Tx.IO.readFile "data/2018/day05.txt"

test :: Tx.Text
test = Tx.pack "dabAcCaCBAcCcaDA"
