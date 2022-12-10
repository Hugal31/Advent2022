module Day10Spec (spec) where

import Advent2022.Day10
import Advent2022.ParseUtils

import Control.Monad.Writer
import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse an instruction" $ do
            runReadP parseInstruction "noop" `shouldBe` Noop
            runReadP parseInstruction "addx 3" `shouldBe` AddX 3

        it "should parse all instructions" $ do
            parse exampleString1 `shouldBe` exampleList1

    describe "instructions" $ do
        it "should run instruction" $ do
            runInstruction Noop 42 `shouldBe` 42
            runInstruction (AddX (-4)) 42 `shouldBe` 38

        it "should run instruction with writer" $ do
            runWriter (runInstructionWithLog Noop 10) `shouldBe` (10, [10])
            runWriter (runInstructionWithLog (AddX 3) 10) `shouldBe` (13, [10, 13])

        it "should run instructions with writer" $ do
            runWriter (runInstructionsWithLog 42 []) `shouldBe` (42, [])
            runWriter (runInstructionsWithLog 10 [Noop, AddX 3]) `shouldBe` (13, [10, 10, 13])

    describe "screen manip" $ do
        it "should compute the drawn coords" $ do
            getDrawnCoords 1 `shouldBe` (0, 0)
            getDrawnCoords 40 `shouldBe` (0, 39)
            getDrawnCoords 41 `shouldBe` (1, 0)
            getDrawnCoords 241 `shouldBe` (0, 0)

        it "should tell if the sprite is drawn" $ do
            isSpriteDrawn 0 0 `shouldBe` True
            isSpriteDrawn 1 0 `shouldBe` True
            isSpriteDrawn 2 0 `shouldBe` False
            isSpriteDrawn 3 5 `shouldBe` False
            isSpriteDrawn 4 5 `shouldBe` True

    describe "solve1" $ do
        it "should solve example 2" $ do
            getCycles exampleList2 !! 18 `shouldBe` 21
            getCycles exampleList2 !! 218 `shouldBe` 18
            getCyclesStrenght [20, 60, 100, 140, 180, 220] (getCycles exampleList2) `shouldBe` [420, 1140, 1800, 2940, 2880, 3960]
            solve1 exampleList2 `shouldBe` 13140

    describe "solve2" $ do
        it "should solve example 2" $ do
            solve2 exampleList2 `shouldBe` exampleList2Solve2Result


exampleString1 = "noop\n\
\addx 3\n\
\addx -5\n"

exampleList1 = [Noop, AddX 3, AddX (-5)]

exampleString2 = "addx 15\n\
\addx -11\n\
\addx 6\n\
\addx -3\n\
\addx 5\n\
\addx -1\n\
\addx -8\n\
\addx 13\n\
\addx 4\n\
\noop\n\
\addx -1\n\
\addx 5\n\
\addx -1\n\
\addx 5\n\
\addx -1\n\
\addx 5\n\
\addx -1\n\
\addx 5\n\
\addx -1\n\
\addx -35\n\
\addx 1\n\
\addx 24\n\
\addx -19\n\
\addx 1\n\
\addx 16\n\
\addx -11\n\
\noop\n\
\noop\n\
\addx 21\n\
\addx -15\n\
\noop\n\
\noop\n\
\addx -3\n\
\addx 9\n\
\addx 1\n\
\addx -3\n\
\addx 8\n\
\addx 1\n\
\addx 5\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\addx -36\n\
\noop\n\
\addx 1\n\
\addx 7\n\
\noop\n\
\noop\n\
\noop\n\
\addx 2\n\
\addx 6\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\noop\n\
\addx 1\n\
\noop\n\
\noop\n\
\addx 7\n\
\addx 1\n\
\noop\n\
\addx -13\n\
\addx 13\n\
\addx 7\n\
\noop\n\
\addx 1\n\
\addx -33\n\
\noop\n\
\noop\n\
\noop\n\
\addx 2\n\
\noop\n\
\noop\n\
\noop\n\
\addx 8\n\
\noop\n\
\addx -1\n\
\addx 2\n\
\addx 1\n\
\noop\n\
\addx 17\n\
\addx -9\n\
\addx 1\n\
\addx 1\n\
\addx -3\n\
\addx 11\n\
\noop\n\
\noop\n\
\addx 1\n\
\noop\n\
\addx 1\n\
\noop\n\
\noop\n\
\addx -13\n\
\addx -19\n\
\addx 1\n\
\addx 3\n\
\addx 26\n\
\addx -30\n\
\addx 12\n\
\addx -1\n\
\addx 3\n\
\addx 1\n\
\noop\n\
\noop\n\
\noop\n\
\addx -9\n\
\addx 18\n\
\addx 1\n\
\addx 2\n\
\noop\n\
\noop\n\
\addx 9\n\
\noop\n\
\noop\n\
\noop\n\
\addx -1\n\
\addx 2\n\
\addx -37\n\
\addx 1\n\
\addx 3\n\
\noop\n\
\addx 15\n\
\addx -21\n\
\addx 22\n\
\addx -6\n\
\addx 1\n\
\noop\n\
\addx 2\n\
\addx 1\n\
\noop\n\
\addx -10\n\
\noop\n\
\noop\n\
\addx 20\n\
\addx 1\n\
\addx 2\n\
\addx 2\n\
\addx -6\n\
\addx -11\n\
\noop\n\
\noop\n\
\noop"

exampleList2 = parse exampleString2

exampleList2Solve2Result = "##..##..##..##..##..##..##..##..##..##..\n\
\###...###...###...###...###...###...###.\n\
\####....####....####....####....####....\n\
\#####.....#####.....#####.....#####.....\n\
\######......######......######......####\n\
\#######.......#######.......#######.....\n"
