{-# LANGUAGE TupleSections #-}
module Advent2022.Day14 where

import Advent2022.Day ( Day, makeDay )
import Advent2022.ParseUtils

import Control.Monad.ST
import GHC.Arr
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)
import Control.Monad (foldM)
import Control.Monad.Loops (takeWhileM)
import Data.List.Split (chunksOf)

type Point = (Int, Int)
type Path = [Point]
type Cell = Bool
type Grid = Array Point Cell

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [Path] -> Int
solve1 = fst . fallWhileInCave . buildGrid

solve2 :: [Path] -> Int
solve2 = fst . fallUntilReachTop . buildGridWithBottom

fallWhileInCave :: Grid -> (Int, Grid)
fallWhileInCave grid = runST $ do
    mutableGrid <- thawSTArray grid
    nRuns <- length <$> takeWhileM (\_ -> fallOne' mutableGrid sandSource) ([0..]::[Int])
    (nRuns,) <$> freezeSTArray mutableGrid

-- TODO Assume inifinite grid?
fallUntilReachTop :: Grid -> (Int, Grid)
fallUntilReachTop grid = runST $ do
    mutableGrid <- thawSTArray grid
    nRuns <- length <$> takeWhileM (\_ -> hasFinished mutableGrid) ([0..]::[Int])
    (nRuns,) <$> freezeSTArray mutableGrid
    where
        isTopFilled g = readSTArray g sandSource
        hasFinished g = (&&) <$> (not <$> isTopFilled g) <*> fallOne' g sandSource

fallOne :: Grid -> (Bool, Grid)
fallOne grid = runST $ do
    mutableGrid <- thawSTArray grid
    inCave <- fallOne' mutableGrid sandSource
    (inCave,) <$> freezeSTArray mutableGrid

fallOne' :: STArray s Point Bool -> Point -> ST s Bool
fallOne' grid point@(y, x) | not (inRange (boundsSTArray grid) point) = return False
                           | otherwise = do
    belowFilled <- safeReadSTArray grid pointBelow
    belowLeftFilled <- safeReadSTArray grid pointBelowLeft
    belowRightFilled <- safeReadSTArray grid pointBelowRight
    if not belowFilled then
        fallOne' grid pointBelow
    else
        if not belowLeftFilled then
            fallOne' grid pointBelowLeft
        else
            if not belowRightFilled then
                fallOne' grid pointBelowRight
            else
                True <$ writeSTArray grid point True
    where
        pointBelow = (y + 1, x)
        pointBelowLeft = (y + 1, x - 1)
        pointBelowRight = (y + 1, x + 1)

-- Return False if not in bounds
safeReadSTArray :: STArray s Point Bool -> Point -> ST s Bool
safeReadSTArray grid point | not (inRange (boundsSTArray grid) point) = return False
                           | otherwise = readSTArray grid point

sandSource :: Point
sandSource = (0, 500)

printGrid :: Grid -> String
printGrid grid = unlines $ chunksOf width $ map cellToChar $ elems grid where
    ((_, xMin), (_, xMax)) = bounds grid
    width = 1 + xMax - xMin
    cellToChar True = '#'
    cellToChar False = ' '

buildGrid :: [Path] -> Grid
buildGrid paths = runST $ do
    grid <- newSTArray bds False
    _ <- fillGrid grid paths
    freezeSTArray grid
    where
    xMin = minimum $ concatMap (map snd) paths
    xMax = maximum $ concatMap (map snd) paths
    yMin = 0
    yMax = maximum $ concatMap (map fst) paths
    bds = ((yMin, xMin), (yMax, xMax))

buildGridWithBottom :: [Path] -> Grid
buildGridWithBottom paths = runST $ do
    grid <- newSTArray bds False
    _ <- fillGrid grid paths
    _ <- drawLine grid ((yMax, xMin'), (yMax, xMax'))
    freezeSTArray grid
    where
    yMin = 0
    yMax = 2 + maximum (concatMap (map fst) paths)
    height = yMax
    xMin = minimum $ concatMap (map snd) paths
    xMax = maximum $ concatMap (map snd) paths
    minWidth = height - 1
    -- I might have take a bit too large here
    xMin' = xMin - minWidth
    xMax' = xMax + minWidth
    bds = ((yMin, xMin'), (yMax, xMax'))

fillGrid :: STArray s Point Cell -> [Path] -> ST s ()
fillGrid grid = foldM (\_ p -> drawPath grid p) ()

drawPath :: STArray s Point Cell -> Path -> ST s ()
drawPath grid path = foldM (\_ p -> drawLine grid p) () (zip path (tail path))

drawLine :: STArray s Point Cell -> (Point, Point) -> ST s ()
drawLine grid ((yBegin, xBegin), (yEnd, xEnd)) | xBegin == xEnd = foldM (\() y -> writeSTArray grid (y, xBegin) True) () (growingRange yBegin yEnd)
                                               | yBegin == yEnd = foldM (\() x -> writeSTArray grid (yBegin, x) True) () (growingRange xBegin xEnd)
                                               | otherwise = error "Invalid diagonal line"

growingRange :: (Enum a, Ord a) => a -> a -> [a]
growingRange a b | a < b = [a..b]
                 | otherwise = [b..a]

-- Parsing

parse :: String -> [Path]
parse = runParsec (parsePaths <* eof)

parsePaths :: Parser [Path]
parsePaths = parsePath `sepEndBy` spaces

parsePath :: Parser Path
parsePath = parseCoordinates `sepBy1` string " -> "

parseCoordinates :: Parser Point
parseCoordinates = flip (,) <$> integer <* char ',' <*> integer
