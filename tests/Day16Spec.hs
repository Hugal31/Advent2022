{-# LANGUAGE FlexibleContexts #-}
module Day16Spec (spec) where

import Advent2022.Day16
import Advent2022.ParseUtils

import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as Map (lookup)
import Test.Hspec
import qualified Data.IntSet as IS
import Data.Maybe (fromJust)
import GHC.Arr ((!))
import Data.Tree (drawForest)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Memo (startEvalMemo)
import Data.Bits (complement)

spec :: Spec
spec = describe "parsing" $ do
    it "should parse valve" $ do
        execParser parseValve "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB" `shouldBe` Valve "AA" 0 ["DD", "II", "BB"]
        execParser parseValve "Valve AB has flow rate=42; tunnel leads to valve DD" `shouldBe` Valve "AB" 42 ["DD"]

    describe "graph" $ do
        it "should build graph" $ do
            let (graph, vertexFromKey) = forceGraphType buildGraph exampleList
                vertexFromKey' = fromJust . vertexFromKey
                in do
                    (vertexFromKey "CC" >>= G.lab graph) `shouldBe` Just 2
                    G.suc graph (vertexFromKey' "CC") `shouldMatchList` [vertexFromKey' "BB", vertexFromKey' "DD"]
                    G.pre graph (vertexFromKey' "CC") `shouldMatchList` [vertexFromKey' "BB", vertexFromKey' "DD"]

        it "should build distance map" $ do
            let (graph, vertexFromKey) = exampleGraphAndMap
                distanceGraph = buildDistanceMap graph
                aa = fromJust $ vertexFromKey "AA"
                bb = fromJust $ vertexFromKey "BB"
                ff = fromJust $ vertexFromKey "FF"
                in do
                    Map.lookup (aa, bb) distanceGraph `shouldBe` Just 1
                    Map.lookup (aa, ff) distanceGraph `shouldBe` Just 3

    describe "runAround" $ do
        let (graph, vertexFromKey) = exampleGraphAndMap
            go v i = runAround graph distanceMap v (fromList i :: BitIntSet)
            vertexFromKey' = fromJust . vertexFromKey
            aa = vertexFromKey' "AA"
            bb = vertexFromKey' "BB"
            cc = vertexFromKey' "CC"
            dd = vertexFromKey' "DD"
            ee = vertexFromKey' "EE" in do
            it "should return 0 when no more time" $ do
                startEvalMemo (go aa [aa, bb, cc, dd, ee] 0) `shouldBe` 0
                startEvalMemo (go ee [aa, bb, cc, dd, ee] 1) `shouldBe` 0

            it "should return the flow rate times the remaining time when only one choice" $ do
                startEvalMemo (go bb [bb] 2) `shouldBe` 13
                startEvalMemo (go bb [bb] 3) `shouldBe` 2 * 13

            it "should choose to open the valve when it's the best solution" $ do
                startEvalMemo (go bb [bb, cc] 3) `shouldBe` 26

            it "should not choose to open the valve when it's the best solution" $ do
                startEvalMemo (go cc [bb, cc] 3) `shouldBe` 13

    describe "bitset" $ do
        it "should behave like intset" $ do
            member 0 (empty::BitIntSet) `shouldBe` False
            member 42 (singleton 42::BitIntSet) `shouldBe` True
            member 42 (singleton 10::BitIntSet) `shouldBe` False
            member 3 (fromList [3, 10]::BitIntSet) `shouldBe` True
            member 3 (delete 3 (singleton 3::BitIntSet)) `shouldBe` False
            Advent2022.Day16.null (empty::BitIntSet) `shouldBe` True
            Advent2022.Day16.null (singleton 3::BitIntSet) `shouldBe` False
            member 2 (fromList [3,6,9,40,4]::BitIntSet) `shouldBe` False
            member 3 (fromList [3,6,9,40,4]::BitIntSet) `shouldBe` True
            toList (singleton 4::BitIntSet) `shouldBe` [4]
            toList (fromList [3,6,9,40,3]::BitIntSet) `shouldBe` [3, 6, 9, 40]
            size (fromList [3,6,9,40,3]::BitIntSet) `shouldBe` 4
            toList (Advent2022.Day16.filter even (fromList [1,2,3,4]::BitIntSet)) `shouldBe` [2,4]

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 1651

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList `shouldBe` 1707

        --it "should generate combinations of human/elephant tasks" $ do
        --    map (bimap toList toList) (listCombinations 2) `shouldMatchList` [([0], [1]), ([1], [0])]

forceGraphType :: (a -> (DefaultGraphType n v , c)) -> (a -> (DefaultGraphType n v, c))
forceGraphType = id

exampleString = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
\Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
\Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
\Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
\Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
\Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
\Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
\Valve HH has flow rate=22; tunnel leads to valve GG\n\
\Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
\Valve JJ has flow rate=21; tunnel leads to valve II\n"

exampleList = parse exampleString

exampleGraphAndMap = forceGraphType buildGraph exampleList
exampleGraph = fst exampleGraphAndMap

distanceMap = buildDistanceMap exampleGraph
