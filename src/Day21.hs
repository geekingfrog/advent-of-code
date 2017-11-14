module Day21 (answer1, answer2) where

import Data.List (nub, minimumBy, maximumBy)
import Data.Ord (comparing)

answer1 :: Int
answer1 =
    let winners = filter ((==Loss) . duelIssue boss) allChara
    in  cost $ minimumBy (comparing cost) winners

answer2 :: Int
answer2 =
    let losers = filter ((==Win) . duelIssue boss) allChara
    in  cost $ maximumBy (comparing cost) losers

data Issue = Win | Loss deriving (Show, Eq)
type Hp = Int
type Atk = Int
type Def = Int
type Cost = Int
type Character = (Hp, Atk, Def, Cost)
type Equipment = (Cost, Atk, Def)

hp (h, _, _, _) = h
atk (_, a, _, _) = a
def (_, _, d, _) = d
cost (_, _, _, c) = c
sumEquip (c1, a1, d1) (c2, a2, d2) = (c1 + c2, a1 + a2, d1 + d2)

weapons =
    map (\(c, a) -> (c, a, 0)) [(8, 4), (10, 5), (25, 6), (40, 7), (74, 8)]
armors =
    map (\(c, d) -> (c, 0, d)) [(13, 1), (31, 2), (53, 3), (75, 4), (102, 5)]
rings =
    [(25, 1, 0), (50, 2, 0), (100, 3, 0), (20, 0, 1), (40, 0, 2), (80, 0, 3)]
boss = (100, 8, 2, 0)

allWeapons = weapons
allArmors = (0, 0, 0) : armors
allRings = (0, 0, 0) : rings ++ nub
    [ sumEquip r1 r2 | r1 <- rings, r2 <- rings, r1 /= r2 ]
allChara =
    [ (100, wa + ra, ad + rd, wc + ac + rc)
    | (wc, wa, _ ) <- allWeapons
    , (ac, _ , ad) <- allArmors
    , (rc, ra, rd) <- allRings
    ]

-- c1 vs c2
duelIssue :: Character -> Character -> Issue
duelIssue c1 c2 =
    let
        dmg1 = max 1 (atk c1 - def c2)
        dmg2 = max 1 (atk c2 - def c1)
        n1   = if hp c2 `mod` dmg1 == 0
            then hp c2 `quot` dmg1
            else hp c2 `quot` dmg1 + 1
        n2 = if hp c1 `mod` dmg2 == 0
            then hp c1 `quot` dmg2
            else hp c1 `quot` dmg2 + 1
    in
        if n1 < n2 then Win else Loss
