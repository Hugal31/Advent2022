module Day09Spec (spec) where

import Prelude hiding (Right, Left)

import Advent2022.Day09
import Advent2022.ParseUtils (runReadP)

import Test.Hspec
import Advent2022.Misc

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse instruction" $ do
            runReadP parseInstruction "R 4" `shouldBe` (Right, 4)
            runReadP parseInstruction "D 1" `shouldBe` (Down, 1)

        it "should parse instructions" $ do
            runReadP parseInstructions "L 2\nU 4\n" `shouldBe` [(Left, 2), (Up, 4)]

    describe "move rope" $ do
        it "should move head" $ do
            moveHead Up [(2, 3), (2, 4)] `shouldBe` [(2, 4), (2, 4)]

        it "should move tail" $ do
            moveTail (2, 3) (2, 1) `shouldBe` (2, 2)
            moveTail (2, 3) (0, 3) `shouldBe` (1, 3)
            moveTail (2, 3) (1, 3) `shouldBe` (1, 3)
            moveTail (2, 1) (0, 0) `shouldBe` (1, 1)
            moveTail (-1, -2) (0, 0) `shouldBe` (-1, -1)

        it "should move tails" $ do
            moveTails [(2, 3), (2, 1)] `shouldBe` [(2, 3), (2, 2)]
            moveTails [(2, 3), (2, 1), (2, 0)] `shouldBe` [(2, 3), (2, 2), (2, 1)]

    describe "apply instructions" $ do
        it "apply one instruction" $ do
            applyInstruction (Right, 4) (initialRope 2) `shouldBe` [initialRope 2, [(1, 0), (0, 0)], [(2, 0), (1, 0)], [(3, 0), (2, 0)], [(4, 0), (3, 0)]]

        it "apply multiple instructions" $ do
            applyInstructions (initialRope 2) [(Right, 2), (Up, 2)] `shouldBe` [initialRope 2, [(1, 0), (0, 0)], [(2, 0), (1, 0)], [(2, 1), (1, 0)], [(2, 2), (2, 1)]]
            applyInstructions (initialRope 3) [(Right, 2), (Up, 2)] `shouldBe` [initialRope 3,
                                                                               [(1, 0), (0, 0), (0, 0)],
                                                                               [(2, 0), (1, 0), (0, 0)],
                                                                               [(2, 1), (1, 0), (0, 0)],
                                                                               [(2, 2), (2, 1), (1, 1)]]

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleList `shouldBe` 13

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList `shouldBe` 1
            solve2 exampleList2 `shouldBe` 36

exampleString = "R 4\n\
\U 4\n\
\L 3\n\
\D 1\n\
\R 4\n\
\D 1\n\
\L 5\n\
\R 2\n"

exampleList = parse exampleString

exampleString2 = "R 5\n\
\U 8\n\
\L 8\n\
\D 3\n\
\R 17\n\
\D 10\n\
\L 25\n\
\U 20\n"

exampleList2 = parse exampleString2
