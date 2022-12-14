module Main where

import Criterion.Main

import Advent2022.Day (Day(Day))
import qualified Advent2022.Days
import Text.Printf (printf)
import Control.Monad

import qualified Day06Bench
import qualified Day11Bench
import qualified Misc

main = do
    days <- benchDays
    daysInputs' <- daysInputs
    defaultMain [
        bgroup "misc" Misc.benches,
        bgroup "days" $ days ++ [
                Day06Bench.benches (daysInputs' !! 5),
                Day11Bench.benches (daysInputs' !! 12)
            ]
        ]

daysInputs :: IO [String]
daysInputs = mapM readDayInput [1..(length Advent2022.Days.days)]

benchDays :: IO [Benchmark]
benchDays = zipWithM benchDay [1..] Advent2022.Days.days

benchDay :: Int -> Day -> IO Benchmark
benchDay dayNumber (Day parseAndSolve1 parseAndSolve2) = do
    content <- readDayInput dayNumber
    return (bgroup (printf "day%02d" dayNumber)
                   [ bench "part1" $ whnf parseAndSolve1 content
                   , bench "part2" $ whnf parseAndSolve2 content ])

readDayInput :: Int -> IO String
readDayInput = readFile . dayInputFilePath

dayInputFilePath :: Int -> String
dayInputFilePath = printf "inputs/day%02d.txt"
