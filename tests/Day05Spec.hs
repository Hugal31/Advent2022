module Day05Spec (spec) where

import Advent2022.Day05
import Text.ParserCombinators.ReadP (readP_to_S, ReadP)
import Test.Hspec

import Data.Bifunctor (second)

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse instruction" $ do
            doParse parseInstruction "move 1 from 2 to 1" `shouldBe` Instruction 1 2 1
            doParse parseInstruction "move 13 from 9 to 8" `shouldBe` Instruction 13 9 8
            doParse parseInstructions "move 1 from 2 to 1\nmove 3 from 1 to 4\n" `shouldBe` [Instruction 1 2 1, Instruction 3 1 4]

        it "should parse row number line" $ do
            doParse parseRowNumberLine " 1   2   3 \n" `shouldBe` [1, 2, 3]

        it "should parse layout" $ do
            doParse parseLayout (unlines $ take 5 $ lines exampleString) `shouldBe` fst exampleList

        it "should flatten maybe list" $ do
            flattenMaybes [Nothing, Just 1, Nothing, Just 2] `shouldBe` [1, 2]

        it "should parse layout line" $ do
            doParse parseLayoutItem "[A]" `shouldBe` Just 'A'
            doParse parseLayoutItem "   " `shouldBe` Nothing
            doParse parseLayoutLine "    [A] [D]" `shouldBe` [Nothing, Just 'A', Just 'D']

        it "should parse cargo" $ do
            doParse parseCargo exampleString `shouldBe` exampleList

        it "should perform instruction" $ do
            applyInstruction9000 (Instruction 1 2 1) ["AB", "C"] `shouldBe` ["CAB", ""]
            applyInstruction9000 (Instruction 2 1 2) ["AB", "C"] `shouldBe` ["", "BAC"]
            applyInstruction9001 (Instruction 2 1 2) ["AB", "C"] `shouldBe` ["", "ABC"]

        it "should add cargo" $ do
            addToCargo 1 "D" ["AB", "C"] `shouldBe` ["DAB", "C"]

        it "should remove cargo" $ do
            removeFromCargo 1 1 ["AB", "C"] `shouldBe` ("A", ["B", "C"])
            removeFromCargo 1 2 ["AB", "C"] `shouldBe` ("AB", ["", "C"])

        it "should solve1" $ do
            solve1 exampleList `shouldBe` "CMZ"
            uncurry resolveCargo1 exampleList `shouldBe` ["C", "M", "ZNDP"]

        it "should solve2" $ do
            solve2 exampleList `shouldBe` "MCD"
            uncurry resolveCargo2 exampleList `shouldBe` ["M", "C", "DNZP"]
            uncurry resolveCargo2 (second reverse exampleList) `shouldNotBe` ["M", "C", "DNZP"]

doParse :: ReadP a -> String -> a
doParse p = fst . last . readP_to_S p

exampleString = "    [D]    \n\
\[N] [C]    \n\
\[Z] [M] [P]\n\
\ 1   2   3 \n\
\\n\
\move 1 from 2 to 1\n\
\move 3 from 1 to 3\n\
\move 2 from 2 to 1\n\
\move 1 from 1 to 2\n"

exampleList :: Cargo
exampleList = ([['N', 'Z'], ['D', 'C', 'M'], ['P']],
               [Instruction 1 2 1, Instruction 3 1 3, Instruction 2 2 1, Instruction 1 1 2])
