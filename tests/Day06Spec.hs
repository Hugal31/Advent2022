module Day06Spec (spec) where

import Advent2022.Day06
import Test.Hspec

spec :: Spec
spec = do
    describe "window" $ do
        it "should return a window of item" $ do
            window 3 [1,2,3,4] `shouldBe` [[1,2,3], [2,3,4]]

        it "should return empty for a list too short" $ do
            window 3 [1,2] `shouldBe` []

    describe "hasDuplicate" $ do
        it "it should return True if a list has duplicate" $ do
            hasDuplicate [1,2,3] `shouldBe` False
            hasDuplicate [1,2,1] `shouldBe` True
            hasDuplicate [2,3,2,1] `shouldBe` True
            hasDuplicate ([]::[Int]) `shouldBe` False

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleString `shouldBe` 7

        it "should solve other examples" $ do
            map solve1 examples `shouldBe` [5, 6, 10, 11]

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleString `shouldBe` 19

exampleString = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

examples = ["bvwbjplbgvbhsrlpgdmjqwftvncz",
            "nppdvjthqldpwncqszvftbrmjlhg",
            "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
            "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]
