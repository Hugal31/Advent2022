module Advent2022.Misc where

import Data.List (sort)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = removeDuplicates' [] . sort

removeDuplicates' :: (Eq a) => [a] -> [a] -> [a]
removeDuplicates' acc [] = acc
removeDuplicates' acc [x] = x : acc
removeDuplicates' acc (a : xs@(b : _)) | a == b = removeDuplicates' acc xs
                                       | otherwise = removeDuplicates' (a:acc) xs

hasDuplicate :: (Ord a) => [a] -> Bool
hasDuplicate = hasDuplicate' . sort

hasDuplicate' :: (Eq a) => [a] -> Bool
hasDuplicate' [] = False
hasDuplicate' [_] = False
hasDuplicate' (a : xs@(b : _)) | a == b = True
                               | otherwise = hasDuplicate' xs
