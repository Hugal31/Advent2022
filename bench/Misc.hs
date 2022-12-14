module Misc (compareFuncs, benches) where

import qualified Advent2022.Day06 (hasDuplicate, window)
import qualified Data.List.Split
import qualified Data.Set

import Criterion

benches :: [Benchmark]
benches = [
    bgroup "hasDuplicate" $
        compareFuncs [ ("sortAndRecurse", whnf Advent2022.Day06.hasDuplicate)
                     , ("set", whnf hasDuplicate') ]
                     [ ("sorted list of 1000", [0..1000])
                     , ("reversed list of 1000", [1000,999..0])
                     , ("two sorted lists of 1000", [0..1000] ++ [0..1000])],
    bgroup "windows" $
        compareFuncs [ ("foldr zipWith", nf (uncurry Advent2022.Day06.window))
                     , ("divvy", nf (\(size, list) -> Data.List.Split.divvy size 1 list))]
                     [ ("10-sized windows of 10000 items", (10, [0..10000]::[Int])) ]
    ]

compareFuncs :: [(String, a -> Benchmarkable)] -> [(String, a)] -> [Benchmark]
compareFuncs funcs args = map (makeGroups args) funcs

makeGroups :: [(String, a)] -> (String, a -> Benchmarkable) -> Benchmark
makeGroups args (name, func) = bgroup name $ map (makeBench func) args

makeBench :: (a -> Benchmarkable) -> (String, a) -> Benchmark
makeBench func (name, arg) = bench name $ func arg

-- From SO
hasDuplicate' list = length list /= length set
  where set = Data.Set.fromList list
