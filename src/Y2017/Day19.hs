module Y2017.Day19 (answer1, answer2) where

import Data.Functor
import Data.Char
import Data.Maybe
import Data.Array as A
import Data.Foldable
import Control.Applicative

answer1 :: IO ()
answer1 = do
    (maze, trail) <- walkMaze
    let letters = mapMaybe (getLetter maze) trail
    putStrLn letters

answer2 :: IO ()
answer2 = do
    (maze, trail) <- walkMaze
    print $ length trail

type Coord = (Int, Int)
type Maze = Array Coord Char
data Direction = U | L | D | R deriving (Show)

walkMaze = do
  maze <- parseInput
  let start = findStart maze
  pure (maze, start : walk maze D start)

findStart grid =
    let ((x0,_), (x1,_)) = A.bounds grid
        Just (x,_) = asum $ map (\x -> get grid (x,0) >>= \v -> Just (x, v))[x0..x1]
     in (x,0)

-- unfoldr doesn't give me the last result so I need this, which is really similar
walk :: Maze -> Direction -> Coord -> [Coord]
walk maze dir c = case nextPos maze dir c of
    Nothing       -> []
    Just (d, c) -> c : walk maze d c

nextPos maze dir c = foldl1 (<|>) $ fmap (\x@(_, c) -> get maze c $> x) [straight c dir, left c dir, right c dir]

getLetter maze c = get maze c >>= (\l -> if ord l >= 65 && ord l < 91 then Just l else Nothing)


straight (x,y) U = (U, (x, y-1))
straight (x,y) L = (L, (x-1, y))
straight (x,y) D = (D, (x, y+1))
straight (x,y) R = (R, (x+1, y))

left (x, y) U = (L, (x - 1, y))
left (x, y) L = (D, (x, y + 1))
left (x, y) D = (R, (x + 1, y))
left (x, y) R = (U, (x, y - 1))

right (x, y) U = (R, (x + 1, y))
right (x, y) L = (U, (x, y - 1))
right (x, y) D = (L, (x - 1, y))
right (x, y) R = (D, (x, y + 1))

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
