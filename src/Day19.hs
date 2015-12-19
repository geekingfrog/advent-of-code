module Day19 (answer1, answer2) where

import Data.Char (isUpper)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe)
import Data.List (nub)

answer1 :: Int
answer1 = length . nub $ replace mapReplacements (split input)

answer2 :: Int
answer2 = let
  allMolecules = iterate (nub . concatMap (replace mapReplacements . split)) ["e"]
  withSteps = zip [0..] allMolecules
  -- hasMedicine xs = input `elem` xs
  in fst . head $ dropWhile (notElem input . snd) withSteps

replaceN n rplList start = iterate (concatMap (replace rplList . split)) start !! n

replace :: M.HashMap String [String] -> [String] -> [String]
replace _ [] = []
replace rplList (x:xs) = let
  candidates = M.lookup x rplList
  notReplaced = map (\el -> x ++ el) (replace rplList xs)
  rest = concat xs
  in case candidates of
       Nothing -> notReplaced
       Just cs -> map (++rest) cs ++ notReplaced

split :: String -> [String]
split s = dropWhile (=="") . reverse $ go s [] [[]]
  where go "" sub acc = reverse sub:acc
        go (c:cs) sub acc = if isUpper c
                            then go cs [c] (reverse sub:acc)
                            else go cs (c:sub) acc

combine :: [[String]] -> [String]
combine ([]) = error "empty list on combine???"
combine ([x]) = x
combine (x:xs) = let
  combined = combine xs
  in concatMap (\el -> map (\c -> el ++ c) combined) x

mapReplacements = M.fromListWith (++) replacements

replacements = [
  ("Al", ["ThF"]),
  ("Al", ["ThRnFAr"]),
  ("B", ["BCa"]),
  ("B", ["TiB"]),
  ("B", ["TiRnFAr"]),
  ("Ca", ["CaCa"]),
  ("Ca", ["PB"]),
  ("Ca", ["PRnFAr"]),
  ("Ca", ["SiRnFYFAr"]),
  ("Ca", ["SiRnMgAr"]),
  ("Ca", ["SiTh"]),
  ("F", ["CaF"]),
  ("F", ["PMg"]),
  ("F", ["SiAl"]),
  ("H", ["CRnAlAr"]),
  ("H", ["CRnFYFYFAr"]),
  ("H", ["CRnFYMgAr"]),
  ("H", ["CRnMgYFAr"]),
  ("H", ["HCa"]),
  ("H", ["NRnFYFAr"]),
  ("H", ["NRnMgAr"]),
  ("H", ["NTh"]),
  ("H", ["OB"]),
  ("H", ["ORnFAr"]),
  ("Mg", ["BF"]),
  ("Mg", ["TiMg"]),
  ("N", ["CRnFAr"]),
  ("N", ["HSi"]),
  ("O", ["CRnFYFAr"]),
  ("O", ["CRnMgAr"]),
  ("O", ["HP"]),
  ("O", ["NRnFAr"]),
  ("O", ["OTi"]),
  ("P", ["CaP"]),
  ("P", ["PTi"]),
  ("P", ["SiRnFAr"]),
  ("Si", ["CaSi"]),
  ("Th", ["ThCa"]),
  ("Ti", ["BP"]),
  ("Ti", ["TiTi"]),
  ("e", ["HF"]),
  ("e", ["NAl"]),
  ("e", ["OMg"])
  ]

input = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

parsedTestInput = drop 2 . reverse $ split testInput
testReplacements = M.fromList [
  ("H", ["HO", "OH"]),
  ("O", ["HH"])
  ]
testInput = "HOH"

testReplacements' = M.fromList [
  ("e", ["H", "O"]),
  ("H", ["HO", "OH"]),
  ("O", ["HH"])
  ]
