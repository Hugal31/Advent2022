module Advent2022.Day08 where

import Advent2022.Day (Day, makeDay)
import Advent2022.Misc (removeDuplicates)

import GHC.Arr (Array, assocs, bounds, listArray, indices, (!))
import Data.List.HT (takeUntil)

type Index = (Int, Int)
type Assoc = (Index, Int)
type Grid = Array Index Int

day :: Day
day = makeDay parse solve1 solve2

solve1 :: Grid -> Int
solve1 = length . removeDuplicates . listVisibles

solve2 :: Grid -> Int
solve2 grid = maximum $ map (scenicScore grid) $ filter (not . isBound bnds) $ indices grid where
    bnds = bounds grid

isBound :: (Index, Index) -> Index -> Bool
isBound ((xMin, yMin), (xMax, yMax)) (x, y) = x == xMin || x == xMax || y == yMin || y == yMax

column :: Int -> Grid -> [Assoc]
column i = filter (\((_, x), _) -> x == i) . assocs

row :: Int -> Grid -> [Assoc]
row i = filter (\((y, _), _) -> y == i) . assocs

scenicScore :: Grid -> Index -> Int
scenicScore grid start@(startX, startY) = upTest * downTest * leftTest * rightTest where
    startHeight = grid ! start
    ((xMin, yMin), (xMax, yMax)) = bounds grid
    upTest = seeTest [(x, startY) | x <- [startX + 1..xMax]]
    downTest = seeTest [(x, startY) | x <- [startX - 1,startX - 2..xMin]]
    rightTest = seeTest [(startX, y) | y <- [startY + 1..yMax]]
    leftTest = seeTest [(startX, y) | y <- [startY - 1,startY - 2..yMin]]
    seeTest = length . takeUntil (>= startHeight) . map (grid !)

listVisibles :: Grid -> [Index]
listVisibles grid = concat [columnTests, rowTests, revColumnTests, revRowColumnTests] where
    ((y1, x1), (y2, x2)) = bounds grid
    columnTests = concatMap (filterVisibles . flip column grid) [x1..x2]
    rowTests = concatMap (filterVisibles . flip row grid) [y1..y2]
    revColumnTests = concatMap (filterVisibles . reverse . flip column grid) [x1..x2]
    revRowColumnTests = concatMap (filterVisibles . reverse . flip row grid) [y1..y2]

-- I'm sure this can be simplified with a monoid or a MonadWriter
filterVisibles :: Ord v => [(i, v)] -> [i]
filterVisibles = map (fst . fst) . filter snd . foldl appendVisible []

appendVisible :: Ord v =>  [((i, v), Bool)] -> (i, v) -> [((i, v), Bool)]
appendVisible acc assoc@(_, v) = (assoc, all ((<v) . snd . fst) acc) : acc

-- Parsing

parse :: String -> Grid
parse s = listArray ((0,0), (size-1, size-1)) (concat ints) where
    ints = map (map (read . (:[]))) (lines s)
    size = length ints
