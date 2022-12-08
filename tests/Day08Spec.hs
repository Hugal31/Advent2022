module Day08Spec (spec) where

import Advent2022.Day08
import Data.List (sort)
import GHC.Arr (bounds, listArray)
import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse intput" $ do
            parse "12\n34\n" `shouldBe` listArray ((0,0), (1,1)) [1..4]
            bounds (parse exampleString) `shouldBe` ((0, 0), (4, 4))

    describe "misc" $ do
        it "column should return a column" $ do
            map snd (column 0 exampleArray) `shouldBe` [3, 2, 6, 3, 3]
            map snd (row 0 exampleArray) `shouldBe` [3, 0, 3, 7, 3]

        it "filterVisible should return the index of the visible trees" $ do
            sort (filterVisibles [(0, 1), (1, 2), (2, 2), (3, 1), (4, 5)]) `shouldBe` [0, 1, 4]

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleArray `shouldBe` 21

exampleString = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390\n"

exampleArray = parse exampleString
