module Y2017.Day25
  ( answer1
  ) where

import Data.Foldable as F

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

runTape =
    let n = 12964419
        (finalTape, finalState) = F.foldl' (\(t, s) _ -> stepTape t s)
                                           (Tape [] False [], A)
                                           [1 .. n]
        tapes = iterate (uncurry stepTape) (Tape [] False [], A)
    in  countTape True finalTape

write :: Tape a -> a -> Tape a
write (Tape xs _ ys) x = Tape xs x ys

right, left :: Tape Bool -> Tape Bool
right (Tape xs e []) = Tape (e:xs) False []
right (Tape xs e (y:ys)) = Tape (e:xs) y ys
left (Tape [] e ys) = Tape [] False (e:ys)
left (Tape (x:xs) e ys) = Tape xs x (e:ys)

countTape :: Eq a => a -> Tape a -> Int
countTape k (Tape xs e ys) =
    Prelude.length (Prelude.filter (==k) xs)
        + Prelude.length (Prelude.filter (==k) ys)
        + if k == e then 1 else 0

stepTape :: Tape Bool -> State -> (Tape Bool, State)
stepTape t@(Tape _ e _) s = case (s, e) of
    (A, False) -> (right $ write t True, B)
    (A, True ) -> (right $ write t False, F)
    (B, False) -> (left $ write t False, B)
    (B, True ) -> (left $ write t True, C)
    (C, False) -> (left $ write t True, D)
    (C, True ) -> (right $ write t False, C)
    (D, False) -> (left $ write t True, E)
    (D, True ) -> (right $ write t True, A)
    (E, False) -> (left $ write t True, F)
    (E, True ) -> (left $ write t False, D)
    (F, False) -> (right $ write t True, A)
    (F, True ) -> (left $ write t False, E)
