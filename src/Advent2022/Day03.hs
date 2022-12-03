module Advent2022.Day03 where

import Advent2022.Day ( Day, makeDay )

import Data.Char (isUpper)
import Data.List (intersect)
import Data.List.Split (chunksOf)

day :: Day
day = makeDay lines solve1 solve2

solve1 :: [String] -> Int
solve1 = sum . map ruckstackPriority

solve2 :: [String] -> Int
solve2 ruckstacks = sum . map (itemPriority . ruckstacksCommon) $ chunksOf 3 ruckstacks

ruckstackPriority :: String -> Int
ruckstackPriority ruckstack = itemPriority $ ruckstackCommon ruckstack

itemPriority :: Char -> Int
itemPriority c
  | isUpper c = 27 + fromEnum c - fromEnum 'A'
  | otherwise = 1 + fromEnum c - fromEnum 'a'

ruckstackCommon :: String -> Char
ruckstackCommon ruckstack = head $ uncurry intersect (splitRuckstack ruckstack)

ruckstacksCommon :: [String] -> Char
ruckstacksCommon [a, b] = head $ intersect a b
ruckstacksCommon (a : b : s) = ruckstacksCommon (a `intersect` b : s)
ruckstacksCommon _ = error "Invalid group"

splitRuckstack :: String -> (String, String)
splitRuckstack ruckstack = splitAt (length ruckstack `div` 2) ruckstack
