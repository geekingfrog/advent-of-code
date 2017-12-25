module Y2017.Day25
  ( answer1
  ) where

import Data.Vector as V
import Data.Foldable as F
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Functor

answer1 :: Int
answer1 =
    let vEnd = runMut
    in V.length $ V.filter id vEnd

data State
    = A
    | B
    | C
    | D
    | E
    | F
     deriving (Show)

runMut =
    runST $
    do let n = 12964419
       -- arbitratry size for the tape, too big and the copying time
       -- gets really big, too small and I get out of bounds exc
       tape <- MV.replicate 50000 False
       foldM'_ (\x _ -> stepMut tape x) (25000, A) (V.fromList [1 .. n])
       V.unsafeFreeze tape

stepMut tape (i, s) = do
    v <- MV.read tape i
    (i', s') <- 
        case (s, v) of
            (A, False) -> MV.write tape i True $> (i + 1, B)
            (A, True) -> MV.write tape i False $> (i + 1, F)
            (B, False) -> MV.write tape i False $> (i - 1, B)
            (B, True) -> MV.write tape i True $> (i - 1, C)
            (C, False) -> MV.write tape i True $> (i - 1, D)
            (C, True) -> MV.write tape i False $> (i + 1, C)
            (D, False) -> MV.write tape i True $> (i - 1, E)
            (D, True) -> MV.write tape i True $> (i + 1, A)
            (E, False) -> MV.write tape i True $> (i - 1, F)
            (E, True) -> MV.write tape i False $> (i - 1, D)
            (F, False) -> MV.write tape i True $> (i + 1, A)
            (F, True) -> MV.write tape i False $> (i - 1, E)
    pure (i', s')
