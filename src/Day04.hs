{-# LANGUAGE OverloadedStrings #-}

module Day04 (answer1, answer2) where

import Data.Digest.Pure.MD5
import Data.ByteString.Builder
import Data.Monoid

answer1 :: String
answer1 = show . fst . head $ filter
    ((=="00000") . take 5 . snd)
    (map (\(i, bs) -> (i, show $ md5 bs)) (zip [0 ..] candidates))

answer2 :: String
answer2 = show . fst . head $ filter
    ((=="000000") . take 6 . snd)
    (map (\(i, bs) -> (i, show $ md5 bs)) (zip [0 ..] candidates))

input = byteString "yzbqklnj"

candidates = map (\d -> toLazyByteString $ input <> (int32Dec d)) [0 ..]
