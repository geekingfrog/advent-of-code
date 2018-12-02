{-# LANGUAGE OverloadedStrings #-}

module Y2015.Day04 (answer1, answer2) where

import Data.Digest.Pure.MD5
import Data.ByteString.Builder
import Data.Monoid

answer1 :: IO ()
answer1 = print . fst . head $ filter
    ((=="00000") . take 5 . snd)
    (map (\(i, bs) -> (i, show $ md5 bs)) (zip [0 ..] candidates))

answer2 :: IO ()
answer2 = print . fst . head $ filter
    ((=="000000") . take 6 . snd)
    (map (\(i, bs) -> (i, show $ md5 bs)) (zip [0 ..] candidates))

input = byteString "yzbqklnj"

candidates = map (\d -> toLazyByteString $ input <> int32Dec d) [0 ..]
