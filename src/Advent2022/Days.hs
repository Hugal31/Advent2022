module Advent2022.Days where

import Advent2022.Day (ParsedSolveFunc, Day, getPart)

import qualified Advent2022.Day01
import qualified Advent2022.Day02
import qualified Advent2022.Day03
import qualified Advent2022.Day04
import qualified Advent2022.Day05
import qualified Advent2022.Day06
import qualified Advent2022.Day07
import qualified Advent2022.Day08
import qualified Advent2022.Day09
import qualified Advent2022.Day10

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
        Advent2022.Day10.day]

getDay :: Int -> Day
getDay day = days !! (day - 1)

getDayPart :: Int -> Int -> ParsedSolveFunc
getDayPart day = getPart (getDay day)
