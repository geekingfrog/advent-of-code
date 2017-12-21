module Y2017.Day21 (answer1, answer2) where

import Data.Maybe
import Data.Array ((!))
import qualified Data.Array as A
import Text.Megaparsec
import Text.Megaparsec.String
import Data.List as L
import Data.List.Split as L

answer1, answer2 :: IO Int
answer1 = do
    rules <- allReplacementRules <$> parseInput
    error "foo"
answer2 = error "wip2"

type A2D a = A.Array (Int, Int) a
type Pattern = A2D Char

start = let Right r = parse parsePattern "start" ".#./..#/###" in r

step rules p =
    let ps = splitPattern p
        replaced = fmap (replacePattern rules) ps
        chunks = L.chunksOf 4 replaced
     in case mergeAll chunks of
          [x] -> x
          _ -> error "not the right number of pattern (should not happen)"

mergeAll :: [[Pattern]] -> [Pattern]
mergeAll [[x]] = [x]
mergeAll ([a,b,c,d]:xs) = mergeAll $ L.chunksOf 4 $ merge a b c d : mergeAll xs
mergeAll _ = error "nope"


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

replacePattern rules p = maybe (error $ "no rules for " ++ show p) snd $ L.find ((==) p . fst) rules

splitPattern p =
    let n = A.rangeSize (A.bounds p)
        r
            | n == 4
            = [p]
            | n == 9
            = [p]
            | otherwise
            = let k       = truncate (sqrt $ fromIntegral n) `div` 2
                  tlSlice = ((0, 0), (k - 1, k - 1))
                  trSlice = ((k, 0), (2 * k - 1, k - 1))
                  blSlice = ((0, k), (k - 1, 2 * k - 1))
                  brSlice = ((k, k), (2 * k - 1, 2 * k - 1))
               in  concatMap (splitPattern . slice p) [tlSlice, trSlice, blSlice, brSlice]
    in  r


slice p s =
    let ((x0, y0), (x1, y1)) = s
        bounds               = ((0, 0), (x1 - x0, y1 - y0))
        elems                = fmap (\x -> p ! x) (A.range s)
    in  A.listArray bounds elems


merge :: Pattern -> Pattern -> Pattern -> Pattern -> Pattern
merge tl tr bl br =
    let bs@(_, (k, _)) = A.bounds tl
        -- ^ size of each pattern to merge
        n              = A.rangeSize bs
        finalBounds    = ((0, 0), (2 * k + 1, 2 * k + 1)) -- (fst (A.bounds tl), snd (A.bounds br))
        getQuadrantAndOffset (x, y) | x <= k && y <= k = (tl, (0, 0))
                                    | x > k && y <= k  = (tr, (-k - 1, 0))
                                    | x <= k && y > k  = (bl, (0, -k - 1))
                                    | otherwise        = (br, (-k - 1, -k - 1))
        elems = do
            c <- A.range finalBounds
            let (p, offset) = getQuadrantAndOffset c
            pure $ p ! (c +: offset)
    in  A.listArray finalBounds elems


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

-- #./.. => .../###/#.#

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


test =
    let Right x = traverse
            (parse parsePattern "")
            [ "#./.."
            , ".#./..#/###"
            , ".../###/#.#"
            , "##.##./#..#../....../##.##./#..#../......"
            ]
    in  x


testRules =
    let Right rs = traverse
            (parse parseLine "")
            ["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"]
     in  allReplacementRules rs
