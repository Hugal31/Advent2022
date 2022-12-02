module Advent2022.Day02 where

import Advent2022.Day (Day, makeDayComplex)

day :: Day
day = makeDayComplex parse1 solve1 parse2 solve2

data Hand = Rock | Paper | Scissors deriving(Enum, Eq, Show)
data Game = Game Hand Hand deriving (Eq, Show)
data GameHint = GameHint Hand Ordering deriving (Eq, Show)

solve1 :: [Game] -> Int
solve1 = sum . map roundScore

solve2 :: [GameHint] -> Int
solve2 = sum . map gameHintScore

playScore :: Hand -> Int
playScore Rock = 1
playScore Paper = 2
playScore Scissors = 3

instance Ord Hand where
  compare Rock Rock = EQ
  compare Paper Paper = EQ
  compare Scissors Scissors = EQ
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare a b = compare EQ $ compare b a

winnerHand :: Hand -> Hand
winnerHand Scissors = Rock
winnerHand hand = succ hand
loserHand :: Hand -> Hand
loserHand Rock = Scissors
loserHand hand = pred hand

guessHand :: GameHint -> Hand
guessHand (GameHint opponent LT) = loserHand opponent
guessHand (GameHint opponent EQ) = opponent
guessHand (GameHint opponent GT) = winnerHand opponent

gameHintScore :: GameHint -> Int
gameHintScore game@(GameHint _ outcome) = roundOutcomeScore outcome + playScore (guessHand game)

roundScore :: Game -> Int
roundScore (Game opponent mine) = playScore mine + roundOutcomeScore (mine `compare` opponent)

roundOutcomeScore :: Ordering -> Int
roundOutcomeScore LT = 0
roundOutcomeScore EQ = 3
roundOutcomeScore GT = 6

-- Parsing

parse1 :: String -> [Game]
parse1 content = map read $ lines content

instance Read Game where
  readsPrec _ (a : ' ' : b : s) = [(Game (read [a]) (read [b]), s)]
  readsPrec _ _ = []

instance Read Hand where
  readsPrec _ ('A' : s) = [(Rock, s)]
  readsPrec _ ('B' : s) = [(Paper, s)]
  readsPrec _ ('C' : s) = [(Scissors, s)]
  readsPrec _ ('X' : s) = [(Rock, s)]
  readsPrec _ ('Y' : s) = [(Paper, s)]
  readsPrec _ ('Z' : s) = [(Scissors, s)]
  readsPrec _ _ = []

parse2 :: String -> [GameHint]
parse2 content = map read $ lines content

instance Read GameHint where
  readsPrec _ (a : ' ' : 'X' : s) = [(GameHint (read [a]) LT, s)]
  readsPrec _ (a : ' ' : 'Y' : s) = [(GameHint (read [a]) EQ, s)]
  readsPrec _ (a : ' ' : 'Z' : s) = [(GameHint (read [a]) GT, s)]
  readsPrec _ _ = []
