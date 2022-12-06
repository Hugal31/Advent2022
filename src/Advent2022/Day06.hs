module Advent2022.Day06 where

import Advent2022.Day (Day, makeDay)
import Data.List (sort, tails)

day :: Day
day = makeDay id solve1 solve2

solve1 :: String -> Int
solve1 = searchNonDupSeq 4

solve2 :: String -> Int
solve2 = searchNonDupSeq 14

searchNonDupSeq :: (Ord a) => Int -> [a] -> Int
searchNonDupSeq n l = n + fst (head $ filter snd $ enumerate windowHaveDuplicate) where
    windows = window n l
    windowHaveDuplicate = map (not . hasDuplicate) windows

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- From https://stackoverflow.com/a/45880037
--hasDuplicates :: (Ord a) => [a] -> Bool
--hasDuplicates list = length list /= length set
--  where set = Set.fromList list

-- TODO Bench
hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate = hasDuplicate' . sort

hasDuplicate' :: (Eq a) => [a] -> Bool
hasDuplicate' [] = False
hasDuplicate' [_] = False
hasDuplicate' (a : xs@(b : _)) | a == b = True
                           | otherwise = hasDuplicate' xs

-- From https://stackoverflow.com/a/27733778
window :: Int -> [a] -> [[a]]
window m = foldr (zipWith (:)) (repeat []) . take m . tails
