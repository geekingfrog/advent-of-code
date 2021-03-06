module Y2015.Day11 (answer1, answer2) where

import Data.List (groupBy, nubBy)

answer1 :: IO ()
answer1 = putStrLn $ head nextPasswords

answer2 :: IO ()
answer2 = putStrLn $ nextPasswords !! 1

nextPasswords = map reverse $ filter validPassword $ iterate next input

input = reverse "vzbxkghb"

next :: String -> String
next []     = "a"
next (c:cs) = let c' = succ c in if c' == '{' then 'a' : next cs else c' : cs

validPassword s = hasPairs s && not (hasForbiddenLetter s) && hasSequence s

hasForbiddenLetter = any (\c -> c == 'i' || c == 'o' || c == 'l')

hasSequence s = checkSequence $ zip3 s (tail s) (tail $ tail s)
  -- remember we're using reversed input
    where checkSequence = any (\(a, b, c) -> a == succ b && b == succ c)

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
