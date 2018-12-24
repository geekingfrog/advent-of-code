{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Y2018.Day13 (answer1, answer2) where

import Prelude hiding (Either(..))

import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Array ((!))
import qualified Data.Array as A

import GHC.Generics
import Control.Lens
import qualified Data.Generics.Product.Fields as GP

answer1, answer2 :: IO ()
answer1 = do
  (grid, carts) <- getData
  let allSteps = iterate (stepCarts grid) (mempty, carts)
  let (Just (crashed, _)) = find ((== 1) . Set.size . fst) allSteps
  print crashed

answer2 = do
  (grid, carts) <- getData
  let n = length carts
  let allSteps = iterate (stepCarts grid) (mempty, carts)
  let (Just (_, [c])) = find ((== 1) . length . snd) allSteps

  print $ pos c
  print c


type Grid = A.Array Coord Char
type Coord = (Int, Int)
data Direction = Left | Up | Down | Right deriving (Show, Eq, Ord)
data Orientation = L | S | R deriving (Show, Eq, Ord)

step :: Grid -> [Cart] -> [Cart]
step grid = fmap (stepCart grid)

stepAndRemove :: Grid -> [Cart] -> Cart
stepAndRemove _ [] = error "no carts left?"
stepAndRemove grid [x] = x
stepAndRemove grid carts =
  let carts' = step grid carts
      nextCarts = case collided carts' of
        [] -> carts'
        crashed -> let s = Set.fromList crashed
                    in filter (\c -> not (c `Set.member` s)) carts'
  in stepAndRemove grid (sortOn (swap . pos) nextCarts)

swap (a,b) = (b,a)

-- list of collided carts
collided :: [Cart] -> [Cart]
collided carts =
  let sorted = sortOn pos carts
      samePos = fst <$> filter (\(a, b) -> pos a == pos b) (zip sorted (tail sorted))
      ps = Set.fromList (fmap pos samePos)
      crashed = filter (\c -> pos c `Set.member` ps) carts
  in crashed

data Cart = Cart
  { pos :: Coord
  , dir :: Direction
  , nextTurn :: Orientation
  }
  deriving (Show, Generic, Eq, Ord)

-- | move all carts, return a list of non crashed carts
-- and a set of crashed positions
stepCarts :: Grid -> (Set.Set Coord, [Cart]) -> (Set.Set Coord, [Cart])
stepCarts grid (_, carts) =
  let go acc [] = acc
      go (carts', positions, crashed) (c:cs)
        | p' `Set.member` positions = go
          (filter ((/= p') . pos) carts', positions, Set.insert p' crashed)
          (filter ((/= p') . pos) cs)

        | otherwise = go
          ( c' : carts'
          , Set.insert p' (Set.delete p positions)
          , crashed
          )
          cs

        where c' = stepCart grid c
              p' = pos c'
              p = pos c

      initialPos = Set.fromList $ fmap pos carts
      (carts', _, crashedPos) = go ([], initialPos, mempty) (sort carts)

   in (crashedPos, carts')

stepCart :: Grid -> Cart -> Cart
stepCart grid c =
  let c' = case grid ! pos c of
        '|' -> advance c
        '-' -> advance c
        '+' -> advance (turnCross c)
        x   -> advance (turn x c)
  in c'

cart = Cart (10, 2) Right L

infixr 5 .+.
(a, b) .+. (c, d) = (a+c, b+d)

infixr 6 .*.
(a, b) .*. (c, d) = (a*c - b*d, a*d + b*c)

advance :: Cart -> Cart
advance c = c & GP.field @"pos" %~ (.+.) (toVec $ dir c)

toVec Left  = (-1, 0)
toVec Up    = (0, -1)
toVec Down  = (0, 1)
toVec Right = (1, 0)

fromVec (-1, 0) = Left
fromVec (0, -1) = Up
fromVec (0, 1) = Down
fromVec (1, 0) = Right
fromVec _ = error "impossible!"

turnCross :: Cart -> Cart
turnCross c =
  let (next, t) = case nextTurn c of
        L -> (S, turnLeft)
        S -> (R, id)
        R -> (L, turnRight)
  in c & GP.field @"dir" %~ t
       & GP.field @"nextTurn" .~ next

turn :: Char -> Cart -> Cart
turn x c = case (x, dir c) of
  ('/',  Left) -> c & GP.field @"dir" .~ Down
  ('/',  Up) -> c & GP.field @"dir" .~ Right
  ('/',  Down) -> c & GP.field @"dir" .~ Left
  ('/',  Right) -> c & GP.field @"dir" .~ Up
  ('\\', Left) -> c & GP.field @"dir" .~ Up
  ('\\', Up) -> c & GP.field @"dir" .~ Left
  ('\\', Down) -> c & GP.field @"dir" .~ Right
  ('\\', Right) -> c & GP.field @"dir" .~ Down
  _ -> error $ "impossibulu! " <> [x] <> " - " <> show c

nextOrientation L = S
nextOrientation S = R
nextOrientation R = L

turnLeft x = fromVec $ toVec x .*. (0, -1)
turnRight x = fromVec $ toVec x .*. (0, 1)

getData :: IO (Grid, [Cart])
getData = do
  raw <- readFile "./data/2018/day13.txt"
  let (vals, carts) = unzip $ do
        (y, l) <- zip [0..] (lines raw)
        (x, c) <- zip [0..] l
        let p = (x,y)
        let (c', mbDir) | c == '^' = ('|', Just Up) -- Just (0,-1))
                          | c == 'v' = ('|', Just Down) -- Just (0,1))
                          | c == '<' = ('-', Just Left) -- Just (-1,0))
                          | c == '>' = ('-', Just Right) -- Just (1,0))
                          | otherwise = (c, Nothing)

        let chart = Cart p <$> mbDir <*> pure L
        pure ((p, c'), chart)
  let maxX = maximum $ fmap (fst . fst) vals
  let maxY = maximum $ fmap (snd . fst) vals
  let grid = A.array ((0,0), (maxX, maxY)) vals
  pure (grid, catMaybes carts)


-- not 110, 21
