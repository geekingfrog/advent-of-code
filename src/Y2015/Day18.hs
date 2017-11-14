module Y2015.Day18 (answer1, answer2) where

import qualified Data.Vector as V
import Control.Monad (liftM)
import Data.Text (chunksOf, pack, unpack)

answer1 :: IO Int
answer1 = do
    initialGrid <- getData
    let states = iterate nextState initialGrid
    return . length . V.filter (==On) $ states !! 100

answer2 :: IO Int
answer2 = do
    initialGrid <- getData
    let states = iterate (nextState . turnCornersOn) initialGrid
    return . length . V.filter (==On) . turnCornersOn $ states !! 100

data LightStatus = On | Off deriving (Eq)
instance Show LightStatus where
  show On = "#"
  show Off = "."

type Grid = V.Vector LightStatus

idxToCoord size i = (i `mod` size, i `quot` size)
coordToIdx size (x, y) = y * size + x

nextState :: Grid -> Grid
nextState g = V.imap (\idx light -> nextStatus light (neighbors g idx)) g

side :: Grid -> Int
side = truncate . sqrt . fromIntegral . length

turnCornersOn :: Grid -> Grid
turnCornersOn g =
    let n       = side g
        corners = [(0, 0), (n - 1, n - 1), (n - 1, 0), (0, n - 1)]
    in  g V.// map (\c -> (coordToIdx n c, On)) corners

neighbors :: Grid -> Int -> [LightStatus]
neighbors g idx =
    let
        n      = side g
        (x, y) = idxToCoord n idx
        coordNeighbors =
            [ (x - 1, y - 1)
            , (x    , y - 1)
            , (x + 1, y - 1)
            , (x - 1, y)
            , (x + 1, y)
            , (x - 1, y + 1)
            , (x    , y + 1)
            , (x + 1, y + 1)
            ]
        indexes = map
            (coordToIdx n)
            ( filter (\(x, y) -> x >= 0 && x < n && y >= 0 && y < n)
                     coordNeighbors
            )
    in
        map (\idx -> g V.! idx) indexes

nextStatus :: LightStatus -> [LightStatus] -> LightStatus
nextStatus current neighbors =
    let nOnNeighbors = length $ filter (==On) neighbors
    in  if current == On
            then (if nOnNeighbors == 2 || nOnNeighbors == 3 then On else Off)
            else if nOnNeighbors == 3 then On else Off

getData :: IO Grid
getData = liftM (V.fromList . map toLightStatus . filter (/='\n'))
                (readFile "./data/18.txt")
  where
    toLightStatus '#' = On
    toLightStatus _   = Off

printGrid :: Grid -> IO ()
printGrid g = do
    let n      = side g
    let l      = pack $ concatMap show $ V.toList g
    let chunks = chunksOf n l
    mapM_ (putStrLn . unpack) chunks
