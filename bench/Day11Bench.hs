module Day11Bench (benches) where

import qualified Advent2022.Day11 (resolveHeap)

import Data.List (sortOn)
import Misc (compareFuncs)
import Criterion

benches :: String -> Benchmark
benches _ = bgroup "day11" [
    bgroup "resolveHeap" $ compareFuncs [ ("resolveHeapPartition", nf Advent2022.Day11.resolveHeap)
                                        , ("resolveHeapSorted", nf resolveHeapSorted)
                                        ]
                                        [ ("reversed list", take 1000 $ cycle [(10, 5), (20, 4), (18, 4), (10, 3), (5, 2), (5, 0), (6, 0)])
                                        , ("sorted list", take 1000 $ cycle [(100, 0), (200, 0), (300, 2), (500, 3), (600, 4), (700, 5)])
                                        ]
    ]

resolveHeapSorted :: [(Int, Int)] -> [[Int]]
resolveHeapSorted heap = reverse $ resolveHeapSorted' [] (sortOn snd heap) 0

resolveHeapSorted' :: [[Int]] -> [(Int, Int)] -> Int -> [[Int]]
resolveHeapSorted' acc [] _ = acc
resolveHeapSorted' acc heap n = resolveHeapSorted' (loot : acc) heap' (n+1) where
    (h, heap') = span ((==n) . snd) heap
    loot = map fst h
