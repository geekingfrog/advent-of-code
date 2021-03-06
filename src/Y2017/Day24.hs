{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

module Y2017.Day24 (answer1, answer2) where

import Data.Monoid
import Data.List.Split as L


answer1, answer2 :: IO ()
answer1 = do
    cs <- parseInput
    let tree = allBridges (0,0) cs
    print $ maxStrength tree
answer2 = do
    cs <- parseInput
    let tree = allBridges (0,0) cs
    print $ snd $ strengthWithLength tree

testTree :: Rose (Int,Int)
testTree = Rose (0,1) [Rose (1,2) [], Rose (1,3) [Rose (3,4) [], Rose (3,3) [], Rose (3,5) []]]

data Rose a = Rose a [Rose a] deriving (Show, Functor, Foldable, Traversable)

sumPair (a,b) = a+b

newtype Add a = Add {unAdd :: a}
    deriving Show

instance Semigroup (Add Int) where
    (Add a) <> (Add b) = Add (a+b)

instance Monoid (Add Int) where
    mempty = Add 0


data RoseG f a = RoseG a (f a) deriving (Show, Functor, Foldable, Traversable)

newtype Max a = Max {unMax :: [a]} deriving Show

allBridges :: (Int, Int) -> [(Int, Int)] -> Rose (Int, Int)
allBridges root@(a,b) xs =
    let candidates = select xs
        okCs = filter (\((x,y),_) -> x == b || y == b) candidates
        swapped = fmap (\e@((x,y),xs) -> if x == b then e else ((y,x),xs)) okCs
        children = fmap (uncurry allBridges) swapped
     in Rose root children

swap (a,b) = (b,a)

maxStrength :: Rose (Int,Int) -> Int
maxStrength (Rose (a,b) []) = a + b
maxStrength (Rose (a,b) cs) = a + b + maximum (fmap maxStrength cs)


strengthWithLength :: Rose (Int, Int) -> (Int,Int)
strengthWithLength (Rose (a,b) []) = (1, a + b)
strengthWithLength (Rose (a,b) cs) =
    let children = fmap strengthWithLength cs
        (l, s) = maximum children
     in (1+l, a+b+s)


select :: [a] -> [(a, [a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y,ys) <- select xs]


parseInput :: IO [(Int, Int)]
parseInput = do
    raw <- readFile "./data/2017/day24.txt"
    let ls = L.splitOn "/" <$> lines raw
    pure $ fmap (\[a,b] -> (read a, read b)) ls


test :: [(Int, Int)]
test = [(0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)]
