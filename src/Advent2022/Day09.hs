module Advent2022.Day09 where

import Prelude hiding (Right, Left)
import Text.ParserCombinators.ReadP
import Advent2022.ParseUtils (parseInt, runReadP)
import Data.List (scanl')
import Advent2022.Misc (removeDuplicates)
import Advent2022.Day (Day, makeDay)

data Direction = Up | Right | Down | Left deriving (Enum, Eq, Ord, Show)
type Instruction = (Direction, Int)
type Position = (Int, Int)
type Offset = (Int, Int)
type Rope = [Position]

day :: Day
day = makeDay parse solve1 solve2

-- TODO Rewrite with a monad writer
solve1 :: [Instruction] -> Int
solve1 = length . removeDuplicates . map last . applyInstructions (initialRope 2)

solve2 :: [Instruction] -> Int
solve2 = length . removeDuplicates . map last . applyInstructions (initialRope 10)

initialRope :: Int -> Rope
initialRope n = replicate n (0, 0)

-- TODO Refactor
applyInstructions :: Rope -> [Instruction] -> [Rope]
applyInstructions r is = concat $ scanl' (\r' i-> tail $ applyInstruction i $ last r') [r] is

applyInstruction :: Instruction -> Rope -> [Rope]
applyInstruction (d, n) = take (n+1) . iterate (moveRope d)

moveRope :: Direction -> Rope -> Rope
moveRope d = moveTails . moveHead d

-- From https://stackoverflow.com/a/66881586, should have taught of that
-- TODO Might be able to use scanl with zip?
moveTails :: Rope -> Rope
moveTails a = reverse (moveTails' [] a)

moveTails' :: Rope -> Rope -> Rope
moveTails' acc (h:t:s) = moveTails' (h : acc) (movedTail : s) where
    movedTail = moveTail h t
moveTails' acc [a] = a : acc
moveTails' acc [] = acc

moveTail :: Position -> Position -> Position
moveTail (hx, hy) t@(tx, ty) = applyOffset (moveTail' (hx - tx, hy - ty)) t

moveTail' :: Offset -> Offset
moveTail' (diffX, diffY) | abs diffX < 2 && abs diffY < 2 = (0, 0)
                         | diffX == 0 = (0, signum diffY)
                         | diffY == 0 = (signum diffX, 0)
                         | otherwise = (signum diffX, signum diffY)

moveHead :: Direction -> Rope -> Rope
moveHead _ [] = []
moveHead d (h:t) = applyDirection d h : t

applyDirection :: Direction -> Position -> Position
applyDirection d = applyOffset (directionToOffset d)

applyOffset :: Offset -> Position -> Position
applyOffset (xOffset, yOffset) (x, y) = (x + xOffset, y + yOffset)

directionToOffset :: Direction -> Offset
directionToOffset Up = (0, 1)
directionToOffset Down = (0, -1)
directionToOffset Right = (1, 0)
directionToOffset Left = (-1, 0)

parse :: String -> [Instruction]
parse = runReadP (parseInstructions <* skipSpaces <* eof)

parseInstructions :: ReadP [Instruction]
parseInstructions = sepBy parseInstruction (char '\n')

parseInstruction :: ReadP Instruction
parseInstruction = (,) <$> (parseDirection <* char ' ') <*> parseInt

parseDirection :: ReadP Direction
parseDirection = choice [Up <$ char 'U'
                        , Right <$ char 'R'
                        , Down <$ char 'D'
                        , Left <$ char 'L']
