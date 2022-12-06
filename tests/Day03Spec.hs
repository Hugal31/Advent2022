module Day03Spec (spec) where

import Advent2022.Day03
import Test.Hspec

import Data.List (intersect)

spec :: Spec
spec = do
    describe "misc" $ do
        it "itemPriority should return the good priority" $ do
            itemPriority 'p' `shouldBe` 16
            itemPriority 'L' `shouldBe` 38
            itemPriority 'v' `shouldBe` 22

        it "should find common character in ruckstack" $ do
            ruckstackCommon "vJrwpWtwJgWrhcsFMMfFFhFp" `shouldBe` 'p'

        it "solve1 on example" $ do
            solve1 exampleList `shouldBe` 157

        it "should find common character between ruckstacks" $ do
            ruckstacksCommon (take 3 exampleList) `shouldBe` 'r'
            ruckstacksCommon (drop 3 exampleList) `shouldBe` 'Z'

        it "solve2 on example" $ do
            solve2 exampleList `shouldBe` 70

exampleText = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw\n"

exampleList = lines exampleText
