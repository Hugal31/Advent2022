module Main where

import Advent2022.Days
import Text.Printf
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    let dayAndPart = parseDayAndPart args
    uncurry runDayAndPart dayAndPart

parseDayAndPart :: [String] -> (Int, Int)
parseDayAndPart [a, b] = (read a :: Int, read b :: Int)
parseDayAndPart _ = error "Invalid args"

runDayAndPart :: Int -> Int -> IO ()
runDayAndPart day part = do
    handle <- openFile (dayInputFile day) ReadMode
    content <- hGetContents handle
    printf "Day %d part %d:\n%d" day part $ Advent2022.Days.getDayPart day part content
    -- Note: I do not close the handle, because otherwise I get issues because I use it lazily.
    -- TODO Look at https://stackoverflow.com/questions/7867723/haskell-file-reading


dayInputFile :: Int -> String
dayInputFile = printf "inputs/day%02d.txt"
