module Day11Spec (spec) where

import Advent2022.Day11
import Advent2022.ParseUtils

import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse items" $ do
            runReadP parseItems "  Starting items: 79, 98" `shouldBe` [79,98]

        it "should parse operator" $ do
            runReadP parseOperator "+" 2 5 `shouldBe` 7
            runReadP parseOperator "*" 2 5 `shouldBe` 10

        it "should parse operand" $ do
            runReadP parseOperand "old" (+) 10 `shouldBe` 20
            runReadP parseOperand "3" (*) 2 `shouldBe` 6

        it "should parse operation" $ do
            runReadP parseOperation "Operation: new = old + old" 5 `shouldBe` 5 + 5
            runReadP parseOperation "Operation: new = old * 19" 5 `shouldBe` 5 * 19

        it "should parse test" $ do
            runReadP parseTestCondition "divisible by 19" `shouldBe` 19
            fst (runReadP parseThrowTest simpleTest) (2*19) `shouldBe` 2
            fst (runReadP parseThrowTest simpleTest) 5 `shouldBe` 0
            snd (runReadP parseThrowTest simpleTest) `shouldBe` 19

        it "should parse a monkey" $ do
            fst (runReadP parseMonkey (unlines $ take 6 $ lines exampleString)) `shouldBe` [79, 98]

        it "should parse" $ do
            length (parse exampleString) `shouldBe` 4

    describe "monkey program" $ do
        it "should throw one" $ do
            throwItem (snd testMonkey1) 79 `shouldBe` (500, 3)
            throwItem (snd testMonkey1) 98 `shouldBe` (620, 3)

        it "should throw all itemss" $ do
            throwItems (snd testMonkey2) [54, 65, 75, 74] `shouldBe` [(20, 0), (23, 0), (27, 0), (26, 0)]

    describe "misc" $ do
        it "should resolve heap" $ do
            resolveHeap [(42, 3), (3, 0), (2, 2), (8, 0)] `shouldBe` [[3, 8], [], [2], [42]]

        it "should merge heap" $ do
            mergeHeapToLootFrom 0 [] [(42, 3), (10, 0)] `shouldBe` [[10], [], [], [42]]

    describe "round program" $ do
        it "should process a round" $ do
            let monkeyLoots = map fst exampleMonkeys
                monkeyPrograms = map snd exampleMonkeys
                in fst (processRound monkeyPrograms monkeyLoots) `shouldBe` [2, 4, 3, 5]

        it "should process rounds" $ do
            let monkeyLoots = map fst exampleMonkeys
                monkeyPrograms = map snd exampleMonkeys
                in fst (processRounds 1 monkeyPrograms monkeyLoots) `shouldBe` [2, 4, 3, 5]
            let monkeyLoots = map fst exampleMonkeysNoRelief
                monkeyPrograms = map snd exampleMonkeysNoRelief
                in do
                    fst (processRounds 1 monkeyPrograms monkeyLoots) `shouldBe` [2, 4, 3, 6]
                    fst (processRounds 20 monkeyPrograms monkeyLoots) `shouldBe` [99, 97, 8, 103]
                    fst (processRounds 1000 monkeyPrograms monkeyLoots) `shouldBe` [5204, 4792, 199, 5192]

    describe "solve1" $ do
        it "should solve example" $ do
            solve1 exampleMonkeysNoRelief `shouldBe` 10605

    describe "solve2" $ do
        it "should solve example" $ do
            solve2 exampleMonkeysNoRelief `shouldBe` 2713310158

simpleTest = "  Test: divisible by 19\n\
\    If true: throw to monkey 2\n\
\    If false: throw to monkey 0"

exampleString = "Monkey 0:\n\
\  Starting items: 79, 98\n\
\  Operation: new = old * 19\n\
\  Test: divisible by 23\n\
\    If true: throw to monkey 2\n\
\    If false: throw to monkey 3\n\
\\n\
\Monkey 1:\n\
\  Starting items: 54, 65, 75, 74\n\
\  Operation: new = old + 6\n\
\  Test: divisible by 19\n\
\    If true: throw to monkey 2\n\
\    If false: throw to monkey 0\n\
\\n\
\Monkey 2:\n\
\  Starting items: 79, 60, 97\n\
\  Operation: new = old * old\n\
\  Test: divisible by 13\n\
\    If true: throw to monkey 1\n\
\    If false: throw to monkey 3\n\
\\n\
\Monkey 3:\n\
\  Starting items: 74\n\
\  Operation: new = old + 3\n\
\  Test: divisible by 17\n\
\    If true: throw to monkey 0\n\
\    If false: throw to monkey 1\n"

exampleMonkeysNoRelief = parse exampleString
exampleMonkeys = map (\(l, (o, b, c)) -> (l, (accountForRelief o, b, c))) exampleMonkeysNoRelief

testMonkey1 = head exampleMonkeys
testMonkey2 = exampleMonkeys !! 1
