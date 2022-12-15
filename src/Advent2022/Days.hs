module Advent2022.Days where

import Advent2022.Day (ParsedSolveFunc, Day, getPart)

import qualified Advent2022.Day01 (day)
import qualified Advent2022.Day02 (day)
import qualified Advent2022.Day03 (day)
import qualified Advent2022.Day04 (day)
import qualified Advent2022.Day05 (day)
import qualified Advent2022.Day06 (day)
import qualified Advent2022.Day07 (day)
import qualified Advent2022.Day08 (day)
import qualified Advent2022.Day09 (day)
import qualified Advent2022.Day10 (day)
import qualified Advent2022.Day11 (day)
import qualified Advent2022.Day12 (day)
import qualified Advent2022.Day13 (day)
import qualified Advent2022.Day14 (day)
import qualified Advent2022.Day15 (day)

days :: [Day]
days = [Advent2022.Day01.day,
        Advent2022.Day02.day,
        Advent2022.Day03.day,
        Advent2022.Day04.day,
        Advent2022.Day05.day,
        Advent2022.Day06.day,
        Advent2022.Day07.day,
        Advent2022.Day08.day,
        Advent2022.Day09.day,
        Advent2022.Day10.day,
        Advent2022.Day11.day,
        Advent2022.Day12.day,
        Advent2022.Day13.day,
        Advent2022.Day14.day,
        Advent2022.Day15.day]

getDay :: Int -> Day
getDay day = days !! (day - 1)

getDayPart :: Int -> Int -> ParsedSolveFunc
getDayPart day = getPart (getDay day)
