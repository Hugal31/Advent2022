{-# LANGUAGE TupleSections #-}
module Advent2022.Day15 where

import Advent2022.Day ( Day, makeDay )
import Advent2022.ParseUtils ( execParser, integer )

import Control.Monad (foldM)
import Data.Range (
    Bound(Bound, boundValue, boundType),
    BoundType(Exclusive, Inclusive),
    Range(SingletonRange, SpanRange, LowerBoundRange, UpperBoundRange, InfiniteRange),
    difference,
    fromRanges,
    mergeRanges,
    (+=+))
import Data.Maybe (listToMaybe, mapMaybe)
import Text.Parsec (sepEndBy, string, endOfLine)
import Text.Parsec.String (Parser)

day :: Day
day = makeDay parse solve1 solve2

-- X, Y
type Point = (Int, Int)
-- Position, beacon
data Sensor = Sensor Point Point deriving (Eq, Show)

sensorPosition :: Sensor -> Point
sensorPosition (Sensor pos _) = pos

solve1 :: [Sensor] -> Int
solve1 = (`countEmptyInRowWithRange` 2000000)

solve2 :: [Sensor] -> Int
solve2 sensors = x * 4000000 + y where
    (x, y) = findUnknownSpot 4000000 sensors

findUnknownSpot :: Int -> [Sensor] -> (Int, Int)
findUnknownSpot maxCoord sensors = head $ mapMaybe (\r -> (,r) <$> getRowEmptySpot r) rows where
    rows = [0..maxCoord]
    constraint = 0 +=+ maxCoord
    getRowEmptySpot row = head . fromRanges <$> foldM (\ranges sensor -> go ranges sensor row) [constraint] sensors
    go ranges sensor row = let newRange = difference ranges (getSensorRangeAtRow sensor row) in if null newRange then Nothing else Just newRange

countEmptyInRow :: [Sensor] -> Int -> Int
countEmptyInRow sensors row = length $ filter id $ mapMaybe (doSensorsTellEmpty sensors) points where
    (minX, maxX) = guessLineRange sensors row
    points = map (,row) [minX..maxX]

countEmptyInRowWithRange :: [Sensor] -> Int -> Int
countEmptyInRowWithRange sensors row = sum $ mapMaybe rangeSize $ mergeRanges $ concatMap (`getSensorEmptyRangeAtRow` row) sensors

-- Return Nothing if infinite
rangeSize :: (Integral i) => Range i -> Maybe i
rangeSize (SingletonRange _) = Just 1
rangeSize (SpanRange Bound {boundValue=a, boundType=Exclusive} Bound {boundValue=b, boundType=Exclusive}) = Just (b - a - 1)
rangeSize (SpanRange Bound {boundValue=a, boundType=Exclusive} Bound {boundValue=b, boundType=Inclusive}) = Just (b - a)
rangeSize (SpanRange Bound {boundValue=a, boundType=Inclusive} Bound {boundValue=b, boundType=Exclusive}) = Just (b - a)
rangeSize (SpanRange Bound {boundValue=a, boundType=Inclusive} Bound {boundValue=b, boundType=Inclusive}) = Just (b - a + 1)
rangeSize (LowerBoundRange _) = Nothing
rangeSize (UpperBoundRange _) = Nothing
rangeSize InfiniteRange = Nothing

-- Given a list of sensor and an Y coordinate, give a X range of the line covered by the sensors
-- Not optimal: may give a bigger range than needed
guessLineRange :: [Sensor] -> Int -> (Int, Int)
guessLineRange sensors row = (minX, maxX) where
    sensorXPositions = map (fst . sensorPosition) sensors
    minSensorX = minimum sensorXPositions
    maxSensorX = maximum sensorXPositions
    maxX = head (filter (\x -> not $ inSensorsRange sensors (x, row)) [maxSensorX..]) - 1
    minX = head (filter (\x -> not $ inSensorsRange sensors (x, row)) [minSensorX,minSensorX-1..]) + 1

doSensorsTellEmpty :: [Sensor] -> Point -> Maybe Bool
doSensorsTellEmpty sensors point = listToMaybe $ mapMaybe isEmpty sensors where
    isEmpty s = doesSensorTellEmpty s point

doesSensorTellEmpty :: Sensor -> Point -> Maybe Bool
doesSensorTellEmpty sensor@(Sensor _ beacon) point | inSensorRange sensor point = Just (point /= beacon)
                                                   | otherwise = Nothing

getSensorEmptyRangeAtRow :: Sensor -> Int -> [Range Int]
getSensorEmptyRangeAtRow sensor@(Sensor _ (bX, bY)) row | row == bY = difference (getSensorRangeAtRow sensor row) [SingletonRange bX]
                                                        | otherwise = getSensorRangeAtRow sensor row

getSensorRangeAtRow :: Sensor -> Int -> [Range Int]
getSensorRangeAtRow sensor@(Sensor (x, y) _) row | semiSpanAtRow >= 0 = [(x - semiSpanAtRow) +=+ (x + semiSpanAtRow)]
                                                 | otherwise = [] where
    yDist = abs (y - row)
    range = sensorRange sensor
    semiSpanAtRow = range - yDist

inSensorsRange :: [Sensor] -> Point -> Bool
inSensorsRange sensors point = any (`inSensorRange` point) sensors

inSensorRange :: Sensor -> Point -> Bool
inSensorRange sensor point = sensorRange sensor >= manhattanDistance (sensorPosition sensor) point

sensorRange :: Sensor -> Int
sensorRange (Sensor s b) = manhattanDistance s b

manhattanDistance :: Point -> Point -> Int
manhattanDistance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

-- Parsing

parse :: String -> [Sensor]
parse = execParser parseSensors

parseSensors :: Parser [Sensor]
parseSensors = parseSensor `sepEndBy` endOfLine

parseSensor :: Parser Sensor
parseSensor = Sensor <$> (string "Sensor at " *> parsePoint) <*> (string ": closest beacon is at " *> parsePoint)

parsePoint :: Parser Point
parsePoint = (,) <$> (string "x=" *> integer) <*> (string ", y=" *> integer)
