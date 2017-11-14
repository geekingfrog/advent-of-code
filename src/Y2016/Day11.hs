module Y2016.Day11 (answer1, answer2) where

import Data.List (groupBy, nubBy)

answer1 :: String
answer1 = head $ nextPasswords

answer2 :: String
answer2 = nextPasswords !! 1

nextPasswords = map reverse $ filter validPassword $ iterate next input

input = reverse "vzbxkghb"

next :: String -> String
next []     = "a"
next (c:cs) = let c' = succ c in if c' == '{' then 'a' : next cs else c' : cs

validPassword s = hasPairs s && not (hasForbiddenLetter s) && hasSequence s

hasForbiddenLetter s = any (\c -> c == 'i' || c == 'o' || c == 'l') s

hasSequence s = checkSequence $ zip3 s (tail s) (tail $ tail s)
  -- remember we're using reversed input
    where checkSequence s' = any (\(a, b, c) -> a == succ b && b == succ c) s'

hasPairs s =
    let pairs     = genPairs s
        noOverlap = filter (uncurry notOverlap) $ zip pairs (tail pairs)
        distincts = filter (\(p1, p2) -> snd p1 /= snd p2) noOverlap
    in  not $ null distincts

genPairs s = map fst $ filter (\((_, a), (_, b)) -> a == b) $ zip
    withPos
    (tail withPos)
    where withPos = zip [0 ..] s

notOverlap p1 p2 = fst p2 > fst p1 + 1
