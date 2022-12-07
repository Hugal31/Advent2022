module Day07Spec (spec) where

import Advent2022.Day07
import Advent2022.ParseUtils

import Data.Tree
import Data.Tree.Zipper
import Test.Hspec
import Data.Maybe

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse command" $ do
            runReadP parseCommand "$ cd /" `shouldBe` CD "/"
            runReadP parseCommand "$cd abcd" `shouldBe` CD "abcd"
            runReadP parseCommand "$ ls" `shouldBe` LS

        it "should parse ls output" $ do
            runReadP parseListing "dir a" `shouldBe` Dir "a"
            runReadP parseListing "12345 a.txt" `shouldBe` Regular "a.txt" 12345

        it "should parse terminal lines" $ do
            runReadP parseTerminalLines "$ cd /\n$ ls\ndir a\n1234 a.txt\n" `shouldBe` [Command (CD "/"), Command LS, File (Dir "a"), File (Regular "a.txt" 1234)]

    describe "filesystem" $ do
        it "should create correct tree" $ do
            buildTree [Command (CD "/"), Command LS, File (Regular "a.txt" 1234), File (Regular "b.txt" 2)]
                `shouldBe` Node {rootLabel=Dir "", subForest=[Node {rootLabel=Regular "a.txt" 1234, subForest=[]}, Node {rootLabel=Regular "b.txt" 2, subForest=[]}]}
            buildTree [Command (CD "/"), Command LS, File (Regular "b.txt" 2), File (Regular "a.txt" 1234)]
                `shouldBe` Node {rootLabel=Dir "", subForest=[Node {rootLabel=Regular "a.txt" 1234, subForest=[]}, Node {rootLabel=Regular "b.txt" 2, subForest=[]}]}
            buildTree [Command (CD "/"), Command (CD "a"), Command LS, File (Regular "b.txt" 2), File (Regular "a.txt" 1234)]
                `shouldBe` Node {rootLabel=Dir "", subForest=[Node {rootLabel=Dir "a", subForest=[Node {rootLabel=Regular "a.txt" 1234, subForest=[]}, Node {rootLabel=Regular "b.txt" 2, subForest=[]}]}]}
            buildTree [Command (CD "c"), Command (CD ".."), Command LS, File (Regular "b.txt" 2), File (Regular "a.txt" 1234)]
                `shouldBe` Node {rootLabel=Dir "", subForest=[Node {rootLabel=Regular "a.txt" 1234, subForest=[]}, Node {rootLabel=Regular "b.txt" 2, subForest=[]}, Node {rootLabel=Dir "c", subForest=[]}]}

    describe "fileTreeSize" $ do
        it "should give file size" $ do
            fileTreeSize (Node {rootLabel=Regular "a.txt" 1234, subForest=[]}) `shouldBe` 1234
            fileTreeSize exampleTree `shouldBe` 48381165

    describe "solve1" $ do
        it "should sove example" $ do
            solve1 exampleList `shouldBe` 95437

    describe "solve2" $ do
        it "should sove example" $ do
            solve2 exampleList `shouldBe` 24933642

exampleString = "$ cd /\n\
\$ ls\n\
\dir a\n\
\14848514 b.txt\n\
\8504156 c.dat\n\
\dir d\n\
\$ cd a\n\
\$ ls\n\
\dir e\n\
\29116 f\n\
\2557 g\n\
\62596 h.lst\n\
\$ cd e\n\
\$ ls\n\
\584 i\n\
\$ cd ..\n\
\$ cd ..\n\
\$ cd d\n\
\$ ls\n\
\4060174 j\n\
\8033020 d.log\n\
\5626152 d.ext\n\
\7214296 k\n"

exampleList = runReadP parseTerminalLines exampleString
exampleTree = buildTree exampleList
