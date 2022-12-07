module Advent2022.Day07 where

import Advent2022.Day (Day, makeDay)
import Advent2022.ParseUtils (getNotWhitespace, parseInt, runReadP)

import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, fromJust)
import Data.Tree
import Data.Tree.Zipper
import Text.ParserCombinators.ReadP

data Command = CD String | LS deriving (Eq, Show)
data File = Dir String | Regular String Int deriving (Eq, Show)
data TerminalLine = Command Command | File File deriving (Eq, Show)

getFileName :: File -> String
getFileName (Dir n) = n
getFileName (Regular n _) = n

getFileSize :: File -> Int
getFileSize (Dir _) = 0
getFileSize (Regular _ s) = s

type FileTree = Tree File
type FileTreePos t = TreePos t File

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [TerminalLine] -> Int
solve1 = sum . filteredFileTreeSize (<=100000) . buildTree

solve2 :: [TerminalLine] -> Int
solve2 tl = minimum $ filteredFileTreeSize (>= needToFree) t where
    t = buildTree tl
    totalSpace = 70000000
    neededSpace = 30000000
    unusedSpace = totalSpace - fileTreeSize t
    needToFree = neededSpace - unusedSpace

filteredFileTreeSize :: (Int -> Bool) -> FileTree -> [Int]
filteredFileTreeSize f t = filter f $ map fileTreeSize $ filter isNodeDir (walkDFS t)

isNodeDir :: FileTree -> Bool
isNodeDir Node { rootLabel=Dir _, subForest=_ } = True
isNodeDir _ = False

-- TODO Find more efficient?
walkDFS :: Tree a -> [Tree a]
walkDFS t = t : concatMap walkDFS (subForest t)

fileTreeSize :: FileTree -> Int
fileTreeSize = foldTree (\f xs -> getFileSize f + sum xs)

buildTree :: [TerminalLine] -> FileTree
buildTree = toTree . foldl' (flip applyLine) (fromTree initialTree)

applyLine :: TerminalLine -> FileTreePos Full -> FileTreePos Full
applyLine (Command command) = applyCommand command
applyLine (File file) = applyListing file

applyCommand :: Command -> FileTreePos Full -> FileTreePos Full
applyCommand LS t = t
applyCommand (CD "/") t = root t
applyCommand (CD "..") t = fromMaybe t (parent t)
applyCommand (CD s) t = insertFile (Dir s) (children t)

applyListing :: File -> FileTreePos Full -> FileTreePos Full
applyListing f t = forceParent $ insertFile f (children t) where
    forceParent = fromJust . parent

insertFile :: File -> FileTreePos Empty -> FileTreePos Full
insertFile f t = case nextTree t of
    Nothing -> insert (Node {rootLabel=f, subForest=[]}) t
    Just t' | getFileName f < getFileName (label t') -> insert (Node {rootLabel=f, subForest=[]}) t
            | getFileName f == getFileName (label t') -> setLabel f t'
            | otherwise -> insertFile f (nextSpace t')

initialTree :: FileTree
initialTree = pure (Dir "")

-- Parsing

parse :: String -> [TerminalLine]
parse = runReadP parseTerminalLines

parseTerminalLines :: ReadP [TerminalLine]
parseTerminalLines = sepBy parseTerminalLine (char '\n') <* skipSpaces <* eof

parseTerminalLine :: ReadP TerminalLine
parseTerminalLine = (Command <$> parseCommand) <++ (File <$> parseListing)

parseCommand :: ReadP Command
parseCommand = parseCD <++ parseLS

parseCD :: ReadP Command
parseCD = CD <$> (parsePrompt *> string "cd " *> skipSpaces *> getNotWhitespace)

parseLS :: ReadP Command
parseLS = LS <$ (parsePrompt *> string "ls")

parsePrompt :: ReadP ()
parsePrompt = const () <$ char '$' *> skipSpaces

parseListing :: ReadP File
parseListing = parseDir <++ parseRegular

parseDir :: ReadP File
parseDir = Dir <$> (string "dir " *> skipSpaces *> getNotWhitespace)

parseRegular :: ReadP File
parseRegular = flip Regular <$> (parseInt <* skipSpaces) <*> getNotWhitespace
