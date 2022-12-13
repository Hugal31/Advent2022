module Advent2022.Day13 where

import Advent2022.Day (Day, makeDay)
import Advent2022.ParseUtils

import Data.Maybe (fromJust)
import Data.List (elemIndex, sort)
import Text.Parsec hiding (parse)
import Text.Parsec.String (Parser)

data Packet = Literal Int | Nested [Packet] deriving (Eq, Show)

instance Ord Packet where
    compare (Literal a) (Literal b) = compare a b
    compare (Nested a) (Nested b) = compare a b
    compare (Literal a) (Nested b) = compare [Literal a] b
    compare (Nested a) (Literal b) = compare a [Literal b]

day :: Day
day = makeDay parse solve1 solve2

solve1 :: [Packet] -> Int
solve1 = sum . map fst . filter isOrdered . zip [1..] . pairs where
  pairs (x:y:xs) = (x, y) : pairs xs
  pairs _ = []
  isOrdered (_, (a, b)) = a < b

solve2 :: [Packet] -> Int
solve2 packets = (1 + fromJust (elemIndex div1 sortedPackets)) * (1 + fromJust (elemIndex div2 sortedPackets)) where
    (div1, div2) = dividerPackets
    sortedPackets = sort $ div1 : div2 : packets

dividerPackets :: (Packet, Packet)
dividerPackets = (Nested [Nested [Literal 2]], Nested [Nested [Literal 6]])

-- Parsing

parse :: String -> [Packet]
parse = runParsec (parsePackets <* eof)

parsePackets :: Parser [Packet]
parsePackets = sepEndBy parsePacket spaces

parsePacket :: Parser Packet
parsePacket = (Literal <$> integer)
          <|> (Nested <$> between (char '[') (char ']') (sepBy parsePacket (char ',')))
