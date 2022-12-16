{-# LANGUAGE FlexibleContexts #-}
module Day16Spec (spec) where

import Advent2022.Day16
import Advent2022.ParseUtils

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
                    vertexFromKey' "CC" `shouldBe` 2
                    (vertexFromKey "CC" >>= G.lab graph) `shouldBe` Just 2
                    G.suc graph (vertexFromKey' "CC") `shouldBe` [vertexFromKey' "BB", vertexFromKey' "DD"]
                    G.pre graph (vertexFromKey' "CC") `shouldBe` [vertexFromKey' "BB", vertexFromKey' "DD"]

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
            go v i = runAround graph distanceMap v (IS.fromList i)
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

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 1651

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
