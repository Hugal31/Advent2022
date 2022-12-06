module Day01Spec (spec) where

import Advent2022.Day01
import Test.Hspec

spec :: Spec
spec = do
    describe "part 1" $ do
        it "can solve example" $ do
            solve1 exampleList `shouldBe` 24000

    describe "part2" $ do
        it "can solve example" $ do
            solve2 exampleList `shouldBe` 45000

    describe "parse" $ do
        it "can parse example" $ do
            parse exampleText `shouldBe` exampleList

exampleText = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000\n"

exampleList :: [[Int]]
exampleList = [[1000, 2000, 3000],
               [4000],
               [5000, 6000],
               [7000, 8000, 9000],
               [10000]]
