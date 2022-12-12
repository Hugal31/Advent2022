{-# LANGUAGE TupleSections #-}
module Advent2022.Day12 where

import Advent2022.Day

import GHC.Arr
import Control.Monad.ST
import Data.Int (Int8)

type Grid e = Array (Int, Int) e

day :: Day
day = makeDay parse solve1 solve2

solve1 :: Grid Char -> Int
solve1 grid = filledGrid ! endPos where
    heightGrid = toHeightGrid grid
    startPos = fst $ head $ filter ((=='S') . snd) $ assocs grid
    endPos = fst $ head $ filter ((=='E') . snd) $ assocs grid
    filledGrid = fillDijkstra canGoUp heightGrid startPos

solve2 :: Grid Char -> Int
solve2 grid = l where
    heightGrid = toHeightGrid grid
    endPos = fst $ head $ filter ((=='E') . snd) $ assocs grid
    distGrid = fillDijkstra canGoDown heightGrid endPos
    lowestPointsAndDist = zip (elems heightGrid) (elems distGrid)
    l = minimum $ map snd $ filter ((==0) . fst) lowestPointsAndDist

toHeightGrid :: Grid Char -> Grid Int8
toHeightGrid = amap toHeight

fillDijkstra :: (Num a, Ord a, Num i, Ix i) => (a -> a -> Bool) -> Array (i, i) a -> (i, i) -> Array (i, i) Int
fillDijkstra canGo grid startIdx = runST $ do
    mutableArray <- newSTArray (bounds grid) maxBound
    _ <- fillDijkstra' canGo grid mutableArray [(startIdx, 0)]
    freezeSTArray mutableArray

fillDijkstra' :: (Num a, Ord a, Num i, Ix i) => (a -> a -> Bool) -> Array (i, i) a -> STArray s (i, i) Int -> [((i, i), Int)] -> ST s ()
fillDijkstra' canGo heighGrid distGrid ((curr, prevDist):toTry) = do
    currDist <- readSTArray distGrid curr
    if currDist > prevDist then do
        _ <- writeSTArray distGrid curr prevDist
        let currHeight = heighGrid ! curr
        let neighbors = map (,prevDist+1) $ filter (\i -> canGo currHeight (heighGrid ! i)) $ listNeighbors curr (bounds heighGrid)
        fillDijkstra' canGo heighGrid distGrid (neighbors ++ toTry)
    else do
        fillDijkstra' canGo heighGrid distGrid toTry
fillDijkstra' _ _ _ [] = pure ()

listNeighbors :: (Num i, Ix i) => (i, i) -> ((i, i), (i, i)) -> [(i, i)]
listNeighbors (y, x) bds = filter (inRange bds) [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

canGoUp :: (Ord a, Num a) => a -> a -> Bool
canGoUp from to = (from + 1) >= to

canGoDown :: (Ord a, Num a) => a -> a -> Bool
canGoDown from to = (from - 1) <= to

parse :: String -> Grid Char
parse s = listArray ((0,0), (height-1, width-1)) (concat chars) where
    chars = lines s
    height = length chars
    width = length (head chars)

toHeight :: Char -> Int8
toHeight 'S' = 0
toHeight 'E' = 25
toHeight c = fromIntegral $ fromEnum c - fromEnum 'a'
