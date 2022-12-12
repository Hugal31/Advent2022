{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Advent2022.Day12 where

import Advent2022.Day

import Control.Monad.ST
import Data.Int (Int8)
import Data.Kind (Type)
import GHC.Arr

-- Actually a n-dimentional grid with rules to move from cell to cell
class (Ix (NodeIndex g)) => Graph g where
    type NodeIndex g :: Type
    bounds :: g -> (NodeIndex g, NodeIndex g)
    -- List the accessible neighbors from a given node
    neighbors :: g -> NodeIndex g -> [NodeIndex g]

type Grid e = Array (Int, Int) e

newtype ClimbGrid e = ClimbGrid (Grid e)
instance (Ord e, Num e) => Graph (ClimbGrid e) where
    type NodeIndex (ClimbGrid e) = (Int, Int)
    bounds (ClimbGrid g) = GHC.Arr.bounds g
    neighbors (ClimbGrid g) i = filter (canGoUp currentHeight . (g !)) (listNeighbors i (GHC.Arr.bounds g)) where
        currentHeight = g ! i

newtype DescendGrid e = DescendGrid (Grid e)
instance (Ord e, Num e) => Graph (DescendGrid e) where
    type NodeIndex (DescendGrid e) = (Int, Int)
    bounds (DescendGrid g) = GHC.Arr.bounds g
    neighbors (DescendGrid g) i = filter (canGoDown currentHeight . (g !)) (listNeighbors i (GHC.Arr.bounds g)) where
        currentHeight = g ! i

day :: Day
day = makeDay parse solve1 solve2

solve1 :: Grid Char -> Int
solve1 grid = distGrid ! endPos where
    heightGrid = toHeightGrid grid
    startPos = fst $ head $ filter ((=='S') . snd) $ assocs grid
    endPos = fst $ head $ filter ((=='E') . snd) $ assocs grid
    distGrid = fillDijkstra (ClimbGrid heightGrid) startPos

solve2 :: Grid Char -> Int
solve2 grid = l where
    heightGrid = toHeightGrid grid
    endPos = fst $ head $ filter ((=='E') . snd) $ assocs grid
    distGrid = fillDijkstra (DescendGrid heightGrid) endPos
    lowestPointsAndDist = zip (elems heightGrid) (elems distGrid)
    l = minimum $ map snd $ filter ((==0) . fst) lowestPointsAndDist

toHeightGrid :: Grid Char -> Grid Int8
toHeightGrid = amap toHeight

fillDijkstra :: Graph g => g -> NodeIndex g -> Array (NodeIndex g) Int
fillDijkstra grid startIdx = runST $ do
    mutableArray <- newSTArray (Advent2022.Day12.bounds grid) maxBound
    _ <- fillDijkstra' grid mutableArray [(startIdx, 0)]
    freezeSTArray mutableArray

fillDijkstra' :: Graph g => g -> STArray s (NodeIndex g) Int -> [(NodeIndex g, Int)] -> ST s ()
fillDijkstra' heighGrid distGrid ((curr, prevDist):toTry) = do
    currDist <- readSTArray distGrid curr
    if currDist > prevDist then do
        _ <- writeSTArray distGrid curr prevDist
        let neighborsToTry = map (,prevDist+1) $ neighbors heighGrid curr
        fillDijkstra' heighGrid distGrid (toTry ++ neighborsToTry)
    else do
        fillDijkstra' heighGrid distGrid toTry
fillDijkstra' _ _ [] = pure ()

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
