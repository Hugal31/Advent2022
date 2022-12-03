module Advent2022.Days where

import Advent2022.Day (ParsedSolveFunc, Day, getPart)

import qualified Advent2022.Day01
import qualified Advent2022.Day02
import qualified Advent2022.Day03

days :: [Day]
days = [Advent2022.Day01.day,
        Advent2022.Day02.day,
        Advent2022.Day03.day]

getDay :: Int -> Day
getDay day = days !! (day - 1)

getDayPart :: Int -> Int -> ParsedSolveFunc
getDayPart day = getPart (getDay day)
