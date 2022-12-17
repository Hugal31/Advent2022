{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Advent2022.Day16 where

import Advent2022.Day (Day, makeDay)
import Advent2022.ParseUtils (execParser, integer)

import Control.Monad.Memo (MonadMemo, memo, startEvalMemo, for3)
import Data.Bits (complement, clearBit, popCount, setBit, shiftL, shiftR, testBit, Bits (bit, popCount, zeroBits, (.&.)))
import Data.Char (isAlpha)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import qualified Data.IntSet as IS
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree
import qualified Data.Graph.Inductive.Query.BFS
import qualified Data.Map.Strict as Map
import Text.Parsec (many1, satisfy, sepBy1, string, sepEndBy, endOfLine, eof, choice)
import Text.Parsec.String (Parser)
import Data.Word (Word64)

data Valve = Valve { name :: String, flow :: Int, tunnels :: [String] } deriving (Eq, Show)

type DefaultGraphType = Data.Graph.Inductive.PatriciaTree.Gr
type DistanceMap = Map.Map (G.Node, G.Node) Int
type Time = Int
type Flow = Int

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [Valve] -> Int
solve1 valves = startEvalMemo (runAround g dm start toVisit 30) where
    (g, kToM) = buildGraph valves :: (DefaultGraphType Int (), String -> Maybe G.Node)
    dm = buildDistanceMap g
    start = fromJust (kToM "AA")
    toVisit = fromList $ map fst $ Prelude.filter (\(_, f) -> f > 0) $ G.labNodes g :: BitIntSet

solve2 :: [Valve] -> Int
solve2 valves = startEvalMemo (maximum <$> results) where
    nUsefulFalves = 1 + length (Prelude.filter ((>0) . flow) valves)
    (g, kToM) = buildGraph valves :: (DefaultGraphType Int (), String -> Maybe G.Node)
    dm = buildDistanceMap g
    start = fromJust (kToM "AA")
    -- A bit tricky: I don't want to filter out useless valves on my listCombinations result,
    -- so I take advantage of the fact I ordered the valves by there usefulness
    combs = listCombinations nUsefulFalves
    search = for3 memo (runAround g dm)
    results = mapM (\(h, e) -> (+) <$> search start h 26 <*> search start e 26) combs

forceLookup :: (Ord k) => k -> Map.Map k v -> v
forceLookup k = fromJust . Map.lookup k

-- List every combination of human/elephant valves, except when one does all the work.
listCombinations :: Int -> [(BitIntSet, BitIntSet)]
listCombinations nValves = map (\i -> (BitIntSet i, BitIntSet (allSet .&. complement i))) [1..(allSet - 1)] where
    allSet = bit nValves - 1

runAround :: (Ord is, IntSetLike is, G.Graph gr, MonadMemo (G.Node, is, Time) Flow m) => gr Int () -> DistanceMap -> G.Node -> is -> Time -> m Flow
runAround _ _ _ _ 0 = return 0
runAround _ _ _ _ 1 = return 0
runAround g dm node toVisit time | size toVisit <= 1 || time == 2 = return flowScore
                                 | otherwise = maximumDefault <$> rests where
    flowScore = fromJust (G.lab g node) * (time - 1)
    distanceToNode n = forceLookup (node, n) dm
    otherNodes = delete node toVisit
    otherNodesAndTime = map (\n -> (n, distanceToNode n)) (toList otherNodes)
    otherNodesReachableInTime = Prelude.filter ((< time + 1) . snd) otherNodesAndTime --Advent2022.Day16.filter (\n -> distanceToNode n < time + 2) otherNodes
    otherNodesReachableInTime' = fromList (map fst otherNodesReachableInTime)
    rests = if flowScore > 0
        then mapM (\(n, d) -> (flowScore +) <$> for3 memo (runAround g dm) n otherNodesReachableInTime' (time - 1 - d)) otherNodesReachableInTime
        else mapM (\(n, d) -> for3 memo (runAround g dm) n otherNodesReachableInTime' (time - d)) otherNodesReachableInTime
    maximumDefault [] = 0
    maximumDefault a = maximum a

buildDistanceMap :: G.Graph gr => gr Int () -> DistanceMap
buildDistanceMap graph = Map.fromList $ concatMap computeDistances $ G.nodes graph where
    computeDistances vertex = map (\(otherVertex, distance) -> ((vertex, otherVertex), distance)) $ Data.Graph.Inductive.Query.BFS.level vertex graph

buildGraph :: G.DynGraph gr => [Valve] -> (gr Int (), String -> Maybe G.Node)
buildGraph valves = let
    orderedValves = sortOn (negate . flow) valves
    keyToNodeDict = Map.fromList $ zip (map name orderedValves) [0..]
    keyToNode k = Map.lookup k keyToNodeDict
    graphWithNodes = G.insNodes (zip [0..] (map flow orderedValves)) G.empty
    addValve Valve {name=n, tunnels=t} = G.insEdges (map (\destName -> (thisNode, fromJust (keyToNode destName), ())) t) where
        thisNode = fromJust (keyToNode n)
    in (foldr addValve graphWithNodes valves, keyToNode)

-- Bitset
class IntSetLike s where
    empty :: s
    singleton :: Int -> s
    fromList :: [Int] -> s
    toList :: s -> [Int]
    insert :: Int -> s -> s
    delete :: Int -> s -> s
    member :: Int -> s -> Bool
    null :: s -> Bool
    null = Prelude.null . toList
    size :: s -> Int
    size = length . toList
    filter :: (Int -> Bool) -> s -> s
    filter f = fromList . Prelude.filter f . toList

instance IntSetLike IS.IntSet where
    empty = IS.empty
    singleton = IS.singleton
    fromList = IS.fromList
    toList = IS.toList
    insert = IS.insert
    delete = IS.delete
    member = IS.member
    null = IS.null
    size = IS.size
    filter = IS.filter

newtype BitIntSet = BitIntSet Word64 deriving (Eq, Ord, Show)

instance IntSetLike BitIntSet where
    empty = BitIntSet 0
    singleton a = BitIntSet (1 `shiftL` a)
    fromList = foldr insert empty
    toList (BitIntSet i) = wordToList i 0
    insert a (BitIntSet i) = BitIntSet (setBit i a)
    delete a (BitIntSet i) = BitIntSet (clearBit i a )
    member a (BitIntSet i) = testBit i a
    null (BitIntSet i) = i == 0
    size (BitIntSet i) = popCount i

wordToList :: Word64 -> Int -> [Int]
wordToList b n | b == zeroBits = []
               | testBit b 0 = n : wordToList (b `shiftR` 1) (n + 1)
               | otherwise = wordToList (b `shiftR` 1) (n + 1)

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
