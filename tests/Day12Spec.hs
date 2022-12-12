module Day12Spec (spec) where

import Advent2022.Day12
import GHC.Arr
import Data.List (sort)

import Test.Hspec

spec :: Spec
spec = do
    describe "misc" $ do
        it "generate safe neighbors indexes" $ do
            sort (listNeighbors (1, 1) ((0, 0), (2, 2))) `shouldBe` sort [(0, 1), (1, 0), (2, 1), (1, 2)]

        it "should tell when a grid can be accessed" $ do
            canGoUp 5 6 `shouldBe` True
            canGoUp 5 7 `shouldBe` False
            canGoUp 5 2 `shouldBe` True

        it "toHeight should give char height" $ do
            toHeight 'a' `shouldBe` 0
            toHeight 'S' `shouldBe` 0
            toHeight 'z' `shouldBe` 25
            toHeight 'E' `shouldBe` 25
            toHeight 'i' `shouldBe` 8

    describe "dijkstra" $ do
        it "should fill a grid with shortest path" $ do
            let grid = listArray ((0,0), (1, 1)) [5, 8, 6, 7] in do
                elems (fillDijkstra canGoUp grid (0, 0)) `shouldBe` [0, 3, 1, 2]

    describe "parse" $ do
        it "should parse example" $ do
            bounds exampleList `shouldBe` ((0, 0), (4, 7))
            exampleList ! (0, 0) `shouldBe` 'S'
            exampleList ! (4, 7) `shouldBe` 'i'

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 31

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList `shouldBe` 29

exampleString = "Sabqponm\n\
\abcryxxl\n\
\accszExk\n\
\acctuvwj\n\
\abdefghi"

exampleList = parse exampleString
