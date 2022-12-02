module Advent2022.Day01 where

import Advent2022.Day
import Data.List (sortBy)
import Data.List.Split

solve1 :: [[Int]] -> Int
solve1 = foldr (max . sum) 0

solve2 :: [[Int]] -> Int
solve2 payload = sum $ take 3 $ sortBy (flip compare) $ map sum payload

parse :: String -> [[Int]]
parse text = map (map read . lines) $ splitSections text

splitSections :: String -> [String]
splitSections = splitOn "\n\n"

day :: Day
day = makeDay parse solve1 solve2
