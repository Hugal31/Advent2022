module Main where

import Criterion.Main

import Advent2022.Day (Day(Day), getPart)
import qualified Advent2022.Days
import Text.Printf (printf)
import Control.Monad

import qualified Misc

main = do
    days <- benchDays
    defaultMain [
        bgroup "misc" Misc.benches,
        bgroup "days" days
        ]

benchDays :: IO [Benchmark]
benchDays = zipWithM benchDay [1..] Advent2022.Days.days

benchDay :: Int -> Day -> IO Benchmark
benchDay dayNumber (Day parseAndSolve1 parseAndSolve2) = do
    content <- readFile (dayInputFilePath dayNumber)
    return (bgroup (printf "day%02d" dayNumber)
                   [ bench "part1" $ whnf parseAndSolve1 content
                   , bench "part2" $ whnf parseAndSolve2 content ])

dayInputFilePath :: Int -> String
dayInputFilePath = printf "inputs/day%02d.txt"
