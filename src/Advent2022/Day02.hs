module Advent2022.Day02 where

import Advent2022.Day (Day, makeDayComplex)

day :: Day
day = makeDayComplex parse1 solve1 parse2 solve2

solve1 :: [Game] -> Int
solve1 = totalRoundScores

solve2 :: [GameHint] -> Int
solve2 = foldr ((+) . gameHintScore) 0

data Play = Rock | Paper | Scissors deriving(Eq, Show)
data Game = Game Play Play deriving (Eq, Show)
data GameHint = GameHint Play Ordering deriving (Eq, Show)

playScore :: Play -> Int
playScore Rock = 1
playScore Paper = 2
playScore Scissors = 3

instance Ord Play where
  compare Rock Rock = EQ
  compare Paper Paper = EQ
  compare Scissors Scissors = EQ
  compare Rock Paper = LT
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare a b = compare EQ $ compare b a

-- TODO Maybe use Enum?
winnerPlay :: Play -> Play
winnerPlay Rock = Paper
winnerPlay Paper = Scissors
winnerPlay Scissors = Rock
loserPlay :: Play -> Play
loserPlay Rock = Scissors
loserPlay Paper = Rock
loserPlay Scissors = Paper

guessPlay :: GameHint -> Play
guessPlay (GameHint opponent LT) = loserPlay opponent
guessPlay (GameHint opponent EQ) = opponent
guessPlay (GameHint opponent GT) = winnerPlay opponent

gameHintScore :: GameHint -> Int
gameHintScore game@(GameHint _ outcome) = roundOutcomeScore outcome + playScore (guessPlay game)

totalRoundScores :: [Game] -> Int
totalRoundScores = foldr ((+) . roundScore) 0

roundScore :: Game -> Int
roundScore (Game opponent mine) = playScore mine + roundOutcomeScore (mine `compare` opponent)

roundOutcomeScore :: Ordering -> Int
roundOutcomeScore LT = 0
roundOutcomeScore EQ = 3
roundOutcomeScore GT = 6

parse1 :: String -> [Game]
parse1 content = map readGame $ lines content

-- TODO This can be improved
readGame :: String -> Game
readGame line = listToGame $ take 2 $ map readPlay $ words line

listToGame :: [Play] -> Game
listToGame [opponent, mine] = Game opponent mine
listToGame _ = error "Invalid game"

-- TODO Implement Read
readPlay :: String -> Play
readPlay "A" = Rock
readPlay "X" = Rock
readPlay "B" = Paper
readPlay "Y" = Paper
readPlay "C" = Scissors
readPlay "Z" = Scissors
readPlay _ = error "Invalid play char"

parse2 :: String -> [GameHint]
parse2 content = map readGameHint $ lines content

-- TODO This can be improved
readGameHint :: String -> GameHint
readGameHint line = listToGameHint $ take 2 $ words line

listToGameHint :: [String] -> GameHint
listToGameHint [a, b] = GameHint (readPlay a) (readOrd b)
listToGameHint _ = error "Invalid game hint"

readOrd :: String -> Ordering
readOrd "X" = LT
readOrd "Y" = EQ
readOrd "Z" = GT
readOrd _ = error "Invalid hint"
