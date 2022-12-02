module Advent2022.Day where

type ParsedSolveFunc = (String -> Int)
data Day = Day ParsedSolveFunc ParsedSolveFunc

makeDay :: (String -> a) -> (a -> Int) -> (a -> Int) -> Day
makeDay parse solve1 solve2 = Day (solve1 . parse) (solve2 . parse)

makeDayComplex :: (String -> a) -> (a -> Int) -> (String -> b) -> (b -> Int) -> Day
makeDayComplex parse1 solve1 parse2 solve2 = Day (solve1 . parse1) (solve2 . parse2)

getPart1 :: Day -> ParsedSolveFunc
getPart1 (Day p _) = p

getPart2 :: Day -> ParsedSolveFunc
getPart2 (Day _ p) = p

getPart :: Day -> Int -> (String -> Int)
getPart day 1 = getPart1 day
getPart day 2 = getPart2 day
getPart _ _ = error "Invalid part"
