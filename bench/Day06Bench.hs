module Day06Bench (benches) where

import Criterion (Benchmark, bgroup, bench, whnf)
import Data.Bits ( Bits(shift, popCount, xor, zeroBits) )
import Data.Int (Int32)
import Data.Foldable (foldr' )

import Advent2022.Day06 (enumerate, window)

benches :: String -> Benchmark
benches input = bgroup "day06" [ bench "part2 with xors" $ whnf solve2Xors input
                               -- , bench "part2 with lookup map" $ whnf solve2Lookup input
                               ]

-- From https://www.reddit.com/r/adventofcode/comments/zdw0u6/comment/iz4l6xk/
solve2Xors :: String -> Int
solve2Xors s = fst $ head $ filter snd $ zip [14..] $ map hasDuplicateXor $ window 14 $ map asciiToInt s where
    asciiToInt c = shift 1 (fromEnum c - fromEnum 'a') :: Int32
    hasDuplicateXor l = popCount (foldr' xor zeroBits l) == length l

-- TODO: implement solve2Lookup from https://www.reddit.com/r/adventofcode/comments/zdw0u6/comment/iz3v100/
