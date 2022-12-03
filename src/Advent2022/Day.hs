module Advent2022.Day where

type ParsedSolveFunc = (String -> Int)
data Day = Day ParsedSolveFunc ParsedSolveFunc

makeDay :: (String -> a) -> (a -> Int) -> (a -> Int) -> Day
makeDay parse solve1 solve2 = Day (solve1 . parse) (solve2 . parse)

makeDayComplex :: (String -> a) -> (a -> Int) -> (String -> b) -> (b -> Int) -> Day
makeDayComplex parse1 solve1 parse2 solve2 = Day (solve1 . parse1) (solve2 . parse2)

getPart :: Day -> Int -> (String -> Int)
getPart (Day p _) 1 = p
getPart (Day _ p) 2 = p
getPart _ _ = error "Invalid part"
