module Day13Spec (spec) where

import Advent2022.Day13
import Advent2022.ParseUtils

import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse packet" $ do
            runParsec parsePacket "1" `shouldBe` Literal 1
            runParsec parsePacket "[]" `shouldBe` Nested []
            runParsec parsePacket "[42]" `shouldBe` Nested [Literal 42]
            runParsec parsePacket "[[],[8],3]" `shouldBe` Nested [Nested [], Nested [Literal 8], Literal 3]

        it "should parse packets" $ do
            runParsec parsePackets "[1]\n[[]]\n\n[]\n[1]" `shouldBe` [Nested [Literal 1], Nested [Nested []], Nested [], Nested [Literal 1]]

    describe "ordering" $ do
        it "should compare packets" $ do
            Literal 1 `compare` Literal 2 `shouldBe` LT
            Literal 5 `compare` Literal 2 `shouldBe` GT
            Literal 2 `compare` Literal 2 `shouldBe` EQ
            Nested [Literal 5] `compare` Nested [Literal 5] `shouldBe` EQ
            Nested [Literal 5] `compare` Nested [Literal 5, Literal 6] `shouldBe` LT
            Literal 5 `compare` Nested [Literal 5, Literal 6] `shouldBe` LT
            Literal 5 `compare` Nested [Literal 5] `shouldBe` EQ
            Nested [Literal 5] `compare` Literal 5 `shouldBe` EQ
            Nested [Literal 5, Literal 6] `compare` Literal 5 `shouldBe` GT

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 13

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList `shouldBe` 140

exampleString = "[1,1,3,1,1]\n\
\[1,1,5,1,1]\n\
\\n\
\[[1],[2,3,4]]\n\
\[[1],4]\n\
\\n\
\[9]\n\
\[[8,7,6]]\n\
\\n\
\[[4,4],4,4]\n\
\[[4,4],4,4,4]\n\
\\n\
\[7,7,7,7]\n\
\[7,7,7]\n\
\\n\
\[]\n\
\[3]\n\
\\n\
\[[[]]]\n\
\[[]]\n\
\\n\
\[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
\[+1,[2,[3,[4,[5,6,0]]]],8,9]\n"

exampleList = parse exampleString
