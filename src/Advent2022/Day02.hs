module Advent2022.Day02 where

import Advent2022.Day (Day, makeDayComplex)
import Advent2022.ParseUtils (runParsec)
import Data.Functor (($>))
import Text.Parsec

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
parse1 = runParsec (parseGames <* eof)

parse2 :: String -> [GameHint]
parse2 = runParsec (parseGameHints <* eof)

parseGames :: Parsec String () [Game]
parseGames = sepEndBy parseGame (char '\n')

parseGame :: Parsec String () Game
parseGame = Game <$> parseHand <* char ' ' <*> parseHand

parseGameHints :: Parsec String () [GameHint]
parseGameHints = sepEndBy parseGameHint newline

parseGameHint :: Parsec String () GameHint
parseGameHint = GameHint <$> parseHand <* char ' ' <*> parseHint

parseHand :: Parsec String () Hand
parseHand = choice [ (char 'A' <|> char 'X') $> Rock
                   , (char 'B' <|> char 'Y') $> Paper
                   , (char 'C' <|> char 'Z') $> Scissors
                   ]

parseHint :: Parsec String () Ordering
parseHint = choice [ char 'X' $> LT
                   , char 'Y' $> EQ
                   , char 'Z' $> GT
                   ]
