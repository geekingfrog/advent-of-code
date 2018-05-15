module Y2017.Day25
  ( answer1
  ) where

import Data.Foldable as F
import Data.Default

answer1 :: Int
answer1 = runTape

data Tape a = Tape [a] a [a]

instance (Show a) => Show (Tape a) where
    show (Tape xs e ys) = unwords (map show $ reverse xs) ++ " [" ++ show e ++ "] " ++ unwords (map show ys)

data State
    = A
    | B
    | C
    | D
    | E
    | F
     deriving (Show)

data Bit = Zero | One deriving (Show, Eq)

instance Default Bit where
    def = Zero

runTape =
    let n = 12964419
        (finalTape, finalState) = F.foldl' (\(t, s) _ -> stepTape t s)
                                           (Tape [] Zero [], A)
                                           [1 .. n]
    in  countTape One finalTape

write :: Tape a -> a -> Tape a
write (Tape xs _ ys) x = Tape xs x ys

right, left :: (Default a) => Tape a -> Tape a
right (Tape xs e []) = Tape (e:xs) def []
right (Tape xs e (y:ys)) = Tape (e:xs) y ys
left (Tape [] e ys) = Tape [] def (e:ys)
left (Tape (x:xs) e ys) = Tape xs x (e:ys)

countTape :: Eq a => a -> Tape a -> Int
countTape k (Tape xs e ys) =
    Prelude.length (Prelude.filter (==k) xs)
        + Prelude.length (Prelude.filter (==k) ys)
        + if k == e then 1 else 0

stepTape :: Tape Bit -> State -> (Tape Bit, State)
stepTape t@(Tape _ e _) s = case (s, e) of
    (A, Zero) -> (right $ write t One, B)
    (A, One ) -> (right $ write t Zero, F)
    (B, Zero) -> (left $ write t Zero, B)
    (B, One ) -> (left $ write t One, C)
    (C, Zero) -> (left $ write t One, D)
    (C, One ) -> (right $ write t Zero, C)
    (D, Zero) -> (left $ write t One, E)
    (D, One ) -> (right $ write t One, A)
    (E, Zero) -> (left $ write t One, F)
    (E, One ) -> (left $ write t Zero, D)
    (F, Zero) -> (right $ write t One, A)
    (F, One ) -> (left $ write t Zero, E)
