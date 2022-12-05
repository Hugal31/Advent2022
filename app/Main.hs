module Main where

import Advent2022.Days
import Text.Printf
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let (day, part) = parseDayAndPart args
    runDayAndPart day part

parseDayAndPart :: [String] -> (Int, Int)
parseDayAndPart [a, b] = (read a, read b)
parseDayAndPart _ = error "Invalid args"

runDayAndPart :: Int -> Int -> IO ()
runDayAndPart day part = do
    content <- readFile (dayInputFile day)
    printf "Day %d part %d: %s\n" day part $ Advent2022.Days.getDayPart day part content

dayInputFile :: Int -> String
dayInputFile = printf "inputs/day%02d.txt"
