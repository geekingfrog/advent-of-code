module Y2017.Day19 (answer1, answer2) where

import Data.Char
import Data.Maybe
import Data.Array as A
import Data.Foldable
import Data.List as L

answer1 :: IO String
answer1 = do
    maze <- parseInput
    let start = findStart maze
    let trail = unfoldr (walk maze) (D, start)
    pure $ catMaybes trail

-- solution: EPYDUXANIT (missing last letter)
-- not EPYDUXANI

answer2 :: IO Int
answer2 = do
    maze <- parseInput
    let start = findStart maze
    let trail = unfoldr (walk maze) (D, start)
    pure $ length trail + 1

type Coord = (Int, Int)
type Maze = Array Coord Char
data Direction = U | L | D | R deriving (Show)


walk :: Maze -> (Direction, Coord) -> Maybe (Maybe Char, (Direction, Coord))
walk maze (d, c) = do
    p' <- nextPos maze d c
    let l = getLetter maze c
    pure (l, p')

findStart grid =
    let ((x0,_), (x1,_)) = A.bounds grid
        Just (x,_) = asum $ map (\x -> get grid (x,0) >>= \v -> Just (x, v))[x0..x1]
     in (x,0)

nextPos :: Maze -> Direction -> Coord -> Maybe (Direction, Coord)
nextPos maze dir c@(x, y) = case get maze $ straight c dir of
    Just _ -> Just (dir, straight c dir)
    Nothing ->
        let a    = left c dir
            b    = right c dir
            ares = (leftD dir, a)
            bres = (rightD dir, b)
        in  case (get maze a, get maze b) of
                (Nothing, Nothing) -> Nothing -- end of maze
                (Just _ , Nothing) -> Just ares
                (Nothing, Just _ ) -> Just bres
                (Just c1, Just c2) ->
                    if c1 == '+' then Just ares else Just bres

getLetter maze c = get maze c >>= (\l -> if ord l >= 65 && ord l < 91 then Just l else Nothing)


straight (x,y) U = (x, y-1)
straight (x,y) L = (x-1, y)
straight (x,y) D = (x, y+1)
straight (x,y) R = (x+1, y)

left (x, y) U = (x - 1, y)
left (x, y) L = (x, y + 1)
left (x, y) D = (x + 1, y)
left (x, y) R = (x, y - 1)

right (x, y) U = (x + 1, y)
right (x, y) L = (x, y - 1)
right (x, y) D = (x - 1, y)
right (x, y) R = (x, y + 1)

straightD d = id
leftD U = L
leftD L = D
leftD D = R
leftD R = U
rightD U = R
rightD L = U
rightD D = L
rightD R = D


parseInput :: IO Maze
parseInput = do
    raw <- readFile "./data/2017/day19.txt"
    -- let raw = test
    let rows = lines raw
        withX = fmap (zip [0..]) rows
        withXY = [((x, y), c) | (y, row) <- zip [0..] withX, (x, c) <- row]
        bounds = ((0,0),(length (head rows)-1, length rows-1))
    pure $ A.array bounds withXY

get grid coord@(x,y) =
    let ((minX, minY), (maxX, maxY)) = A.bounds grid
     in if x < minX || x > maxX || y < minY || y > maxY
           then Nothing
           else case grid ! coord of
                  ' ' -> Nothing
                  c -> Just c


test = unlines
    [ "     |          "
    , "     |  +--+    "
    , "     A  |  C    "
    , " F---|----E|--+ "
    , "     |  |  |  D "
    , "     +B-+  +--+ "
    ]
