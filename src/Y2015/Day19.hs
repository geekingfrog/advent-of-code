module Y2015.Day19 (answer1, answer2) where

import Data.Char (isUpper)
import qualified Data.HashMap.Strict as M
import Data.Maybe (fromMaybe, isJust, fromJust, catMaybes)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)
import Control.Arrow (second)
import Data.Algorithms.KMP (build, match)
import Control.Monad (fmap)

data Rose a = Rose a [Rose a] deriving (Show, Eq)

answer1 :: IO ()
answer1 = print $ length . nub $ replace mapReplacements (split input)

answer2 :: IO ()
answer2 =
    let rose = reductions replacements' input
        err  = error "No solution???"
    in  print $ fromMaybe err (roseDepth rose)

replace :: M.HashMap String [String] -> [String] -> [String]
replace _ [] = []
replace rplList (x:xs) =
    let candidates  = M.lookup x rplList
        notReplaced = map (\el -> x ++ el) (replace rplList xs)
        rest        = concat xs
    in  case candidates of
            Nothing -> notReplaced
            Just cs -> map (++rest) cs ++ notReplaced

split :: String -> [String]
split s = dropWhile (=="") . reverse $ go s [] [[]]
  where
    go "" sub acc = reverse sub : acc
    go (c:cs) sub acc =
        if isUpper c then go cs [c] (reverse sub : acc) else go cs (c : sub) acc

mapReplacements = M.fromListWith (++) replacements

findFarthestReplacements
    :: [(String, String)] -> String -> [((String, String), Int)]
findFarthestReplacements rplList input =
    let
        pair rpl = (rpl, match (build $ snd rpl) input)
        pairs  = map pair rplList
        purged = foldl
            (\acc (p, l) -> if null l then acc else (p, maximum l) : acc)
            []
            pairs
        maxIdx     = maximum $ map snd purged
        candidates = filter ((==) maxIdx . snd) purged
    in
        if null purged then [] else candidates

replaceOne :: String -> ((String, String), Int) -> String
replaceOne s ((out, origin), idx) =
    take idx s ++ out ++ drop (idx + length origin) s

reduce :: [(String, String)] -> String -> [String]
reduce rplList input =
    map (replaceOne input) (findFarthestReplacements rplList input)

reductions :: [(String, String)] -> String -> Rose String
reductions rplList s = Rose s (map (reductions rplList) (reduce rplList s))

roseDepth :: Rose String -> Maybe Int
roseDepth (Rose s []) = if s == "e" then Just 0 else Nothing
roseDepth (Rose s xs) = fmap (+1) (minimumMay . catMaybes $ map roseDepth xs)

minimumMay [] = Nothing
minimumMay xs = Just $ minimum xs

replacements =
    [ ("Al", ["ThF"])
    , ("Al", ["ThRnFAr"])
    , ("B" , ["BCa"])
    , ("B" , ["TiB"])
    , ("B" , ["TiRnFAr"])
    , ("Ca", ["CaCa"])
    , ("Ca", ["PB"])
    , ("Ca", ["PRnFAr"])
    , ("Ca", ["SiRnFYFAr"])
    , ("Ca", ["SiRnMgAr"])
    , ("Ca", ["SiTh"])
    , ("F" , ["CaF"])
    , ("F" , ["PMg"])
    , ("F" , ["SiAl"])
    , ("H" , ["CRnAlAr"])
    , ("H" , ["CRnFYFYFAr"])
    , ("H" , ["CRnFYMgAr"])
    , ("H" , ["CRnMgYFAr"])
    , ("H" , ["HCa"])
    , ("H" , ["NRnFYFAr"])
    , ("H" , ["NRnMgAr"])
    , ("H" , ["NTh"])
    , ("H" , ["OB"])
    , ("H" , ["ORnFAr"])
    , ("Mg", ["BF"])
    , ("Mg", ["TiMg"])
    , ("N" , ["CRnFAr"])
    , ("N" , ["HSi"])
    , ("O" , ["CRnFYFAr"])
    , ("O" , ["CRnMgAr"])
    , ("O" , ["HP"])
    , ("O" , ["NRnFAr"])
    , ("O" , ["OTi"])
    , ("P" , ["CaP"])
    , ("P" , ["PTi"])
    , ("P" , ["SiRnFAr"])
    , ("Si", ["CaSi"])
    , ("Th", ["ThCa"])
    , ("Ti", ["BP"])
    , ("Ti", ["TiTi"])
    , ("e" , ["HF"])
    , ("e" , ["NAl"])
    , ("e" , ["OMg"])
    ]

replacements' = map (second head) replacements

input
    = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

parsedTestInput = drop 2 . reverse $ split testInput
testReplacements = M.fromList [("H", ["HO", "OH"]), ("O", ["HH"])]
testInput = "HOH"

testReplacements' =
    M.fromList [("e", ["H", "O"]), ("H", ["HO", "OH"]), ("O", ["HH"])]

-- 201 too high
