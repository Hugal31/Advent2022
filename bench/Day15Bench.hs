module Day15Bench (benches) where

import Criterion (Benchmark, bgroup, bench, nf)
import Data.Bits ( Bits(shift, popCount, xor, zeroBits) )
import Data.Int (Int32)
import Data.Foldable (foldr' )

import Advent2022.Day15 (countEmptyInRow, countEmptyInRowWithRange, parse)

import Misc (compareFuncs)

benches :: String -> Benchmark
benches input = bgroup "day15" [
        bgroup "countEmptyInRow" $ compareFuncs [("countEmptyInRow with iteration", nf (`countEmptyInRow` 2000000))
                                                ,("countEmptyInRow with ranges", nf (`countEmptyInRowWithRange` 2000000))]
                                                -- I don't know if the seq here work. I want the parsing to be fully done before running the benchmarks
                                                [inputList `seq` ("input", inputList)]
    ]
    where
        inputList = parse input
