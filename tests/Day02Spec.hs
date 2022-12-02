module Day02Spec where

import Advent2022.Day02
import Test.Hspec

spec :: Spec
spec = do
    describe "play" $ do
        it "gives the right score" $ do
            playScore Rock `shouldBe` 1
            playScore Paper `shouldBe` 2
            playScore Scissors `shouldBe` 3

        it "wins correctly" $ do
            Rock > Paper `shouldBe` False
            Rock > Scissors `shouldBe` True
            Scissors < Rock `shouldBe` True
            Paper > Rock `shouldBe` True
            Paper < Paper `shouldBe` False
            Paper == Paper `shouldBe` True
            Paper `compare` Paper `shouldBe` EQ
            Scissors `compare` Paper `shouldBe` GT

    describe "rounds" $ do
        it "round outcome gives the right score" $ do
            roundOutcomeScore LT `shouldBe` 0
            roundOutcomeScore EQ `shouldBe` 3
            roundOutcomeScore GT `shouldBe` 6

        it "round gives the right score" $ do
            roundScore (Game Rock Paper) `shouldBe` 8
            roundScore (Game Paper Rock) `shouldBe` 1
            roundScore (Game Scissors Scissors) `shouldBe` 6

    describe "game hint" $ do
        it "should guess right play" $ do
            guessHand (GameHint Rock EQ) `shouldBe` Rock
            guessHand (GameHint Paper LT) `shouldBe` Rock
            guessHand (GameHint Scissors GT) `shouldBe` Rock

    describe "parse" $ do
        it "parse1 example" $ do
            parse1 exampleText `shouldBe` exampleList1
        it "parse2 example" $ do
            parse2 exampleText `shouldBe` exampleList2

    describe "solve 1 gives the right score for multiple rounds" $ do
        it "should solve example" $ do
            solve1 exampleList1 `shouldBe` 15

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleList2 `shouldBe` 12


exampleText = "A Y\n\
\B X\n\
\C Z\n"

exampleList1 = [Game Rock Paper, Game Paper Rock, Game Scissors Scissors]
exampleList2 = [GameHint Rock EQ, GameHint Paper LT, GameHint Scissors GT]
