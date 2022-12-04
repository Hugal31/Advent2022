module Day04Spec where

import Advent2022.Day04
import Test.Hspec

spec :: Spec
spec = do
    describe "misc" $ do
        it "doesPairHaveContainedRange should return True if the pair has one range fully containing the other" $ do
            doesRangesHaveContainedRange (Range 2 4) (Range 6 8) `shouldBe` False
            doesRangesHaveContainedRange (Range 2 8) (Range 3 7) `shouldBe` True
            doesRangesHaveContainedRange (Range 6 6) (Range 4 6) `shouldBe` True

        it "doesRangesOverlap should return true if the two pairs overlaps" $ do
            doesRangesOverlap (Range 2 4) (Range 6 8) `shouldBe` False
            doesRangesOverlap (Range 5 7) (Range 7 9) `shouldBe` True

    describe "parse" $ do
        it "should parse example" $ do
            parse exampleString `shouldBe` exampleList

    describe "read Range" $ do
        it "should parse simple range" $ do
            read "1-3" `shouldBe` Range 1 3

    describe "solve1" $ do
        it "should work on example" $ do
            solve1 exampleList `shouldBe` 2

    describe "solve2" $ do
        it "should work on example" $ do
            solve2 exampleList `shouldBe` 4

exampleString = "2-4,6-8\n\
\2-3,4-5\n\
\5-7,7-9\n\
\2-8,3-7\n\
\6-6,4-6\n\
\2-6,4-8\n"

exampleList = [(Range 2 4, Range 6 8),
               (Range 2 3, Range 4 5),
               (Range 5 7, Range 7 9),
               (Range 2 8, Range 3 7),
               (Range 6 6, Range 4 6),
               (Range 2 6, Range 4 8)]
