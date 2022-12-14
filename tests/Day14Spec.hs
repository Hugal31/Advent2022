module Day14Spec (spec) where

import Advent2022.Day14
import Advent2022.ParseUtils

import GHC.Arr
import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse coordinates" $ do
            execParser parseCoordinates "493,5" `shouldBe` (5, 493)

        it "should parse path" $ do
            execParser parsePath "493,5 -> 123,5 -> 123,6" `shouldBe` [(5, 493), (5, 123), (6, 123)]

        it "should parsePaths" $ do
            execParser parsePaths exampleString `shouldBe` exampleList

    describe "grid" $ do
        it "should build grid" $ do
            bounds (buildGrid exampleList) `shouldBe` ((0, 494), (9, 503))
            exampleGrid ! (0, 500) `shouldBe` False
            exampleGrid ! (4, 498) `shouldBe` True

        it "should print grid" $ do
            printGrid exampleGrid `shouldBe` printedGrid

    describe "sand" $ do
        it "should fall correctly" $ do
            exampleGrid ! (8, 500) `shouldBe` False
            fst (fallOne exampleGrid) `shouldBe` True
            snd (fallOne exampleGrid) ! (8, 500) `shouldBe` True

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 24

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList `shouldBe` 93

exampleString = "498,4 -> 498,6 -> 496,6\n\
\503,4 -> 502,4 -> 502,9 -> 494,9"

exampleList = [[(4, 498), (6, 498), (6, 496)], [(4, 503), (4, 502), (9, 502), (9, 494)]]
exampleGrid = buildGrid exampleList

printedGrid = "          \n\
\          \n\
\          \n\
\          \n\
\    #   ##\n\
\    #   # \n\
\  ###   # \n\
\        # \n\
\        # \n\
\######### \n"
