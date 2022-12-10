module Advent2022.Day10 where

import Control.Monad.ST
import Control.Monad.Writer
import Text.ParserCombinators.ReadP
import Advent2022.ParseUtils (parseInt, runReadP)
import Data.Foldable (foldl')
import GHC.Arr
import Advent2022.Day (Day, makeDay)
import Data.List.Split (chunksOf)

data Instruction = Noop | AddX Int deriving (Eq, Show)
type Index = (Int, Int)
type Screen s = STArray s Index Bool

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [Instruction] -> Int
solve1 instructions = sum $ getCyclesStrenght [20, 60, 100, 140, 180, 220] (getCycles instructions)

-- TODO Try Renderable,Graphics.Rendering.Chart.Grid
solve2 :: [Instruction] -> String
solve2 instructions = displayScreen $ runST $ do
    screen <- newScreen
    mapM_ (\(cycl, regX) -> applyCycleToScreen cycl regX screen) (zip [1..] $ 1 : getCycles instructions)
    freezeSTArray screen

displayScreen :: Array Index Bool -> String
displayScreen screen = unlines $ chunksOf 40 $ map litBool (elems screen) where
    litBool False = '.'
    litBool True = '#'

newScreen :: ST s (Screen s)
newScreen = newSTArray ((0, 0), (5, 39)) False

applyCycleToScreen :: Int -> Int -> Screen s -> ST s ()
applyCycleToScreen cycl regX screen = when (isSpriteDrawn x regX) (litPixel (y, x) screen) where
    (y, x) = getDrawnCoords cycl

litPixel :: Index -> Screen s -> ST s ()
litPixel idx screen = writeSTArray screen idx True

isSpriteDrawn :: Int -> Int -> Bool
isSpriteDrawn xCoord regX = abs (xCoord - regX) <= 1

getDrawnCoords :: Int -> Index
getDrawnCoords c = (c' `div` 40 `mod` 6, c' `mod` 40) where c' = c - 1

getCyclesStrenght :: [Int] -> [Int] -> [Int]
getCyclesStrenght idx cycles = map cycleStrength idx where
    cycleStrength x = x * (cycles !! (x - 2))

getCycles :: [Instruction] -> [Int]
getCycles instructions = snd $ runWriter $ runInstructionsWithLog 1 instructions

runInstructionsWithLog :: Int -> [Instruction] -> Writer [Int] Int
runInstructionsWithLog reg instructions = pipeM (pure reg) (map runInstructionWithLog instructions)

pipeM :: (Monad m) => m a -> [a -> m a] -> m a
pipeM = foldl' (>>=)

runInstructionWithLog :: Instruction -> Int -> Writer [Int] Int
runInstructionWithLog Noop x = writer (x, [x])
runInstructionWithLog (AddX n) x = writer (x', [x, x']) where x' = x + n

-- Try a monadic way
runInstruction :: Instruction -> Int -> Int
runInstruction Noop x = x
runInstruction (AddX n) x = x + n

-- Parsing

parse :: String -> [Instruction]
parse = runReadP (parseInstructions <* skipSpaces <* eof)

parseInstructions :: ReadP [Instruction]
parseInstructions = sepBy parseInstruction (char '\n')

parseInstruction :: ReadP Instruction
parseInstruction = (Noop <$ string "noop") <++ (AddX <$> (string "addx " *> parseInt))
