module Y2017.Day21 (answer1, answer2) where

import Data.Maybe
import Data.Void
import Data.Array ((!))
import qualified Data.Array as A
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List as L

type Parser = Parsec Void String

answer1, answer2 :: IO ()
answer1 = solve 5 >>= print
answer2 = solve 18 >>= print -- 2766750 in ~30s


type A2D a = A.Array (Int, Int) a
type Pattern = A2D Char

solve n = do
    rules <- allReplacementRules <$> parseInput
    let images     = iterate (step rules) start
        finalImage = images !! n
    pure $ length $ filter (=='#') $ A.elems finalImage

start = let Right r = parse parsePattern "start" ".#./..#/###" in r

step rules p =
    let ps       = splitPattern p
        replaced = fmap (replacePattern rules) ps
    in  merge replaced

allReplacementRules :: [(Pattern, Pattern)] -> [(Pattern, Pattern)]
allReplacementRules =
    concatMap (\(start, end) -> zip (flipAndRotate start) (repeat end))

flipAndRotate :: Pattern -> [Pattern]
flipAndRotate p =
    L.nub $ concat [ take 4 $ iterate rotate x | x <- [p, flipX p, flipY p] ]

-- rotate 90° counter clockwise
rotate :: Pattern -> Pattern
rotate p =
    let b@(_, (x, y)) = A.bounds p
        rotate2       = A.array
            b
            [ ((0, 0), p ! (1, 0))
            , ((1, 0), p ! (1, 1))
            , ((0, 1), p ! (0, 0))
            , ((1, 1), p ! (0, 1))
            ]
        rotate3 =
            let assoc = do
                    i <- A.indices p
                    let (x', y') = i -: (1, 1)
                    let rotated  = (-y', x') +: (1, 1)
                    pure (i, p ! rotated)
            in  A.array b assoc
    in  if x == 1 then rotate2 else rotate3


-- flip along X axis
flipX :: Pattern -> Pattern
flipX p =
    let b@(_, (nx, ny)) = A.bounds p
        flip2           = A.array
            b
            [ ((0, 0), p ! (0, 1))
            , ((1, 0), p ! (1, 1))
            , ((0, 1), p ! (0, 0))
            , ((1, 1), p ! (1, 0))
            ]
        flip3 = A.array b $ do
            i <- A.indices p
            let (x, y) = i -: (1, 1)
            let i'     = (x, -y) +: (1, 1)
            pure (i, p ! i')
    in  if nx == 1 then flip2 else flip3

flipY p = rotate $ rotate $ flipX p

replacePattern rules p =
    maybe (error $ "no rules for " ++ show p) snd $ L.find ((==) p . fst) rules

splitPattern p =
    let
        n    = A.rangeSize (A.bounds p)
        size = truncate (sqrt $ fromIntegral n)
        r
            | n == 4
            = [p]
            | n == 9
            = [p]
            | otherwise
            = let (s, k) = if even size
                      then (2, size `div` 2)
                      else (3, size `div` 3)
                  slices = do
                      y0 <- [0, s .. size - 1]
                      x0 <- [0, s .. size - 1]
                      let x1 = x0 + s - 1
                          y1 = y0 + s - 1
                      pure ((x0, y0), (x1, y1))
              in  fmap (slice p) slices
    in
        r


slice p s =
    let ((x0, y0), (x1, y1)) = s
        bounds               = ((0, 0), (x1 - x0, y1 - y0))
        elems                = fmap (\x -> p ! x) (A.range s)
    in  A.listArray bounds elems


merge :: [Pattern] -> Pattern
merge [x] = x
merge ps =
    let
        n            = length ps
        size         = truncate (sqrt $ fromIntegral n)
        (_, (k', _)) = A.bounds (head ps)
        k            = k' + 1
        -- k = if even size then 2 else 3
        offsets =
            [ (x * k, y * k) | y <- [0 .. size - 1], x <- [0 .. size - 1] ]
        offsetPattern p o = fmap (\(x, c) -> (x +: o, c)) (A.assocs p)
        as     = concat $ zipWith offsetPattern ps offsets
        bounds = ((0, 0), (k * size - 1, k * size - 1))
    in
        A.array bounds as
 -- in as


(a, b) +: (c, d) = (a + c, b + d)
(a, b) -: (c, d) = (a - c, b - d)


parseInput :: IO [(Pattern, Pattern)]
parseInput = do
    raw <- readFile "./data/2017/day21.txt"
    case parse (parseLine `sepEndBy` newline) "day 21" raw of
        Left  err -> error $ show err
        Right x   -> pure x


parseLine = do
    start <- parsePattern
    string " => "
    end <- parsePattern
    pure (start, end)

parsePattern :: Parser Pattern
parsePattern = do
    lines <- many (char '#' <|> char '.') `sepBy1` char '/'
    let n      = length lines
    let bounds = ((0, 0), (n - 1, n - 1))
    let rows   = fmap (zip [0 ..]) lines
    let assoc  = [ ((x, y), c) | (y, row) <- zip [0 ..] rows, (x, c) <- row ]
    pure $ A.array bounds assoc


pprint :: Pattern -> IO ()
pprint p = do
    let (_, (nx, ny)) = A.bounds p
    let getRow y = [ p ! (x, y) | x <- [0 .. nx] ]
    let rows = [ getRow y | y <- [0 .. ny] ]
    mapM_ putStrLn rows
    putStrLn ""


testRules =
    let Right rs = traverse
            (parse parseLine "")
            ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]
    in  allReplacementRules rs
