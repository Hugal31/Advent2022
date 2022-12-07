module Advent2022.Day05 where

import Advent2022.Day
import Advent2022.ParseUtils (prefixedInt, parseInt)

import Text.ParserCombinators.ReadP
import Data.Foldable (foldl')
import Data.Functor ( ($>) )
import Data.List (transpose)

type Column = [Char]
type Layout = [Column]
data Instruction = Instruction Int Int Int deriving (Eq, Show)
type Instructions = [Instruction]
type Cargo = (Layout, Instructions)

day :: Day
day = makeDay parse solve1 solve2

solve1 :: Cargo -> String
solve1 cargo = readTopRow $ uncurry resolveCargo1 cargo

resolveCargo1 :: Layout -> Instructions -> Layout
resolveCargo1 = foldl' (flip applyInstruction9000)

solve2 :: Cargo -> String
solve2 cargo = readTopRow $ uncurry resolveCargo2 cargo

resolveCargo2 :: Layout -> Instructions -> Layout
resolveCargo2 = foldl' (flip applyInstruction9001)

readTopRow :: Layout -> String
readTopRow = map head

applyInstruction9000 :: Instruction -> Layout -> Layout
applyInstruction9000 (Instruction amount from to) layout = uncurry (addToCargo to . reverse) $ removeFromCargo from amount layout

applyInstruction9001 :: Instruction -> Layout -> Layout
applyInstruction9001 (Instruction amount from to) layout = uncurry (addToCargo to) $ removeFromCargo from amount layout

-- Not a big fan
addToCargo :: Int -> Column -> Layout -> Layout
addToCargo to cargo layout = x ++ s' : xs where
    (x, s, xs) = splitAt3 to layout
    s' = addToColumn cargo s

removeFromCargo :: Int -> Int -> Layout -> (Column, Layout)
removeFromCargo from amount layout = (remainder, x ++ s':xs) where
    (x, s, xs) = splitAt3 from layout
    (remainder, s') = removeFromColumn amount s

addToColumn :: Column -> Column -> Column
addToColumn = (++)

removeFromColumn :: Int -> Column -> (Column, Column)
removeFromColumn = splitAt

-- TODO Improve?
splitAt3 :: Int -> [a] -> ([a], a, [a])
splitAt3 n l = case splitAt (n-1) l of
                    (x, s' : xs') -> (x, s', xs')
                    _ -> error "Invalid layout or column"

parse :: String -> Cargo
parse = fst . last . readP_to_S parseCargo

parseCargo :: ReadP Cargo
parseCargo = do
    layout <- parseLayout
    _ <- skipSpaces
    instructions <- parseInstructions
    return (layout, instructions)

parseLayout :: ReadP Layout
parseLayout = do
    rows <- sepBy1 parseLayoutLine (char '\n')
    _ <- skipSpaces
    _ <- parseRowNumberLine
    return (map flattenMaybes $ transpose rows)

-- I'm sure there's a monadic way
flattenMaybes :: [Maybe a] -> [a]
flattenMaybes = flattenMaybes' []

flattenMaybes' :: [a] -> [Maybe a] -> [a]
flattenMaybes' acc [] = reverse acc
flattenMaybes' acc (Just x : xs) = flattenMaybes' (x : acc) xs
flattenMaybes' acc (Nothing : xs) = flattenMaybes' acc xs

parseLayoutLine :: ReadP [Maybe Char]
parseLayoutLine = sepBy parseLayoutItem (char ' ')

parseLayoutItem :: ReadP (Maybe Char)
parseLayoutItem = (Just <$> between (char '[') (char ']') get) <++ (string "   " $> Nothing)

parseRowNumberLine :: ReadP [Int]
parseRowNumberLine = optional (char ' ') *> sepBy1 parseInt (many1 $ char ' ')

parseInstructions :: ReadP Instructions
parseInstructions = sepBy parseInstruction (char '\n') <* optional (char '\n')

parseInstruction :: ReadP Instruction
parseInstruction = Instruction <$> prefixedInt "move " <*> prefixedInt " from " <*> prefixedInt " to "
