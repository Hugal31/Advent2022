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

rpad :: [a] -> Int -> a -> [a]
rpad a n d = if missing > 0 then a ++ replicate missing d else a where
    l = length a
    missing = n - l

zipWithDefaultR :: (a -> b -> c) -> b -> [a] -> [b] -> [c]
zipWithDefaultR f bDefault (a:as) (b:bs) = f a b : zipWithDefaultR f bDefault as bs
zipWithDefaultR f bDefault (a:as) [] = f a bDefault : zipWithDefaultR f bDefault as []
zipWithDefaultR _ _ [] _ = []

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault f def (a:as) (b:bs) = f a b : zipWithDefault f def as bs
zipWithDefault f def (a:as) [] = f a def : zipWithDefault f def as []
zipWithDefault f def [] (b:bs) = f b def : zipWithDefault f def [] bs
zipWithDefault _ _ [] [] = []
