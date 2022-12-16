{-# LANGUAGE FlexibleContexts #-}
module Advent2022.Day16 where

import Advent2022.Day (Day, makeDay)
import Advent2022.ParseUtils (execParser, integer)

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo, for3)
import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import qualified Data.IntSet as IS
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.BFS
import qualified Data.Map.Strict as Map
import Text.Parsec (many1, satisfy, sepBy1, string, sepEndBy, endOfLine, eof, choice)
import Text.Parsec.String (Parser)
import Debug.Trace

data Valve = Valve { name :: String, flow :: Int, tunnels :: [String] } deriving (Eq, Show)

type DefaultGraphType = Data.Graph.Inductive.PatriciaTree.Gr
type DistanceMap = Map.Map (G.Node, G.Node) Int
type Time = Int
type Flow = Int

day :: Day
day = makeDay parse solve1 solve1

solve1 :: [Valve] -> Int
solve1 valves = startEvalMemo (runAround g dm start toVisit 30) where
    (g, kToM) = buildGraph valves :: (DefaultGraphType Int (), String -> Maybe G.Node)
    dm = buildDistanceMap g
    start = fromJust (kToM "AA")
    toVisit = IS.fromList $ map fst $ filter (\(_, f) -> f > 0) $ G.labNodes g

forceLookup :: (Ord k) => k -> Map.Map k v -> v
forceLookup k = fromJust . Map.lookup k

testConcat :: (Monad m) => m [a] -> m [a] -> m [a]
testConcat a b = (++) <$> a <*> b

runAround :: (G.Graph gr, MonadMemo (G.Node, IS.IntSet, Time) Flow m) => gr Int () -> DistanceMap -> G.Node -> IS.IntSet -> Time -> m Flow
runAround _ _ _ _ 0 = return 0
runAround _ _ _ _ 1 = return 0
runAround g dm node toVisit time | IS.size toVisit <= 1 || time == 2 = return flowScore
                                 | otherwise = maximumConcat otherNodesIfOpened otherNodesIfNotOpened where
    flowScore = fromJust (G.lab g node) * (time - 1)
    distanceToNode n = forceLookup (node, n) dm
    otherNodes = IS.delete node toVisit
    otherNodesReachableInTime = IS.filter (\n -> distanceToNode n < time + 2) otherNodes
    otherNodesIfOpened = if flowScore > 0 then
        mapM (\n -> (flowScore +) <$> for3 memo (runAround g dm) n otherNodes (time - 1 - distanceToNode n)) (IS.toList otherNodesReachableInTime)
        else return []
    otherNodesIfNotOpened = mapM (\n -> for3 memo (runAround g dm) n toVisit (time - distanceToNode n)) (IS.toList otherNodesReachableInTime)
    maximumConcat a b = maximumDefault <$> ((++) <$> a <*> b)
    maximumDefault [] = 0
    maximumDefault a = maximum a

buildDistanceMap :: G.Graph gr => gr Int () -> DistanceMap
buildDistanceMap graph = Map.fromList $ concatMap computeDistances $ G.nodes graph where
    computeDistances vertex = map (\(otherVertex, distance) -> ((vertex, otherVertex), distance)) $ Data.Graph.Inductive.Query.BFS.level vertex graph

buildGraph :: G.DynGraph gr => [Valve] -> (gr Int (), String -> Maybe G.Node)
buildGraph valves = let
    keyToNodeDict = Map.fromList $ zip (map name valves) [0..]
    keyToNode k = Map.lookup k keyToNodeDict
    graphWithNodes = G.insNodes (zip [0..] (map flow valves)) G.empty
    addValve Valve {name=n, tunnels=t} = G.insEdges (map (\destName -> (thisNode, fromJust (keyToNode destName), ())) t) where
        thisNode = fromJust (keyToNode n)
    in (foldr addValve graphWithNodes valves, keyToNode)

-- Parsing

parse :: String -> [Valve]
parse = execParser (parseValves <* eof)

parseValves :: Parser [Valve]
parseValves = parseValve `sepEndBy` endOfLine

parseValve :: Parser Valve
parseValve = Valve
    <$> (string "Valve " *> parseName)
    <*> (string " has flow rate=" *> integer)
    <*> (string "; tunnel" *> choice [string "s lead to valves ", string " leads to valve "] *> (parseName `sepBy1` string ", "))
    where parseName = many1 (satisfy isAlpha)
