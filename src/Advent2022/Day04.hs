module Advent2022.Day04 where

import Advent2022.Day (Day, makeDay)
--import GHC.Read (Read(readPrec), expectP)
--import Text.Read (ReadPrec, get, pfail)
--import qualified Text.Read.Lex as L
import Data.Bifunctor (bimap)

day :: Day
day = makeDay parse solve1 solve2

data Range = Range Int Int deriving (Eq, Show)
type Pair = (Range, Range)

solve1 :: [Pair] -> Int
solve1 pairs = length $ filter (uncurry doesRangesHaveContainedRange) pairs

solve2 :: [Pair] -> Int
solve2 pairs = length $ filter (uncurry doesRangesOverlap) pairs

doesRangesHaveContainedRange :: Range -> Range -> Bool
doesRangesHaveContainedRange range1 range2 = doesRangesContainsOther range1 range2 || doesRangesContainsOther range2 range1

doesRangesContainsOther :: Range -> Range -> Bool
doesRangesContainsOther (Range beg1 end1) (Range beg2 end2) = beg1 <= beg2 && end1 >= end2

doesRangesOverlap :: Range -> Range -> Bool
doesRangesOverlap (Range beg1 end1) (Range beg2 end2) = beg1 <= end2 && end1 >= beg2

-- Parsing

-- newType for parsing
newtype ParsingPair = ParsingPair { getPair :: Pair }

parse :: String -> [Pair]
parse content = map (getPair . read) $ lines content

-- TODO: improve, maybe use readPrec
instance Read ParsingPair where
    readsPrec _ str = [(ParsingPair (bimap read (read . tail) $ break (==',') str), "")]

-- TODO: improve, maybe use readPrec
instance Read Range where
    {- Terrible: use read inside of readsPrec, hardcode the ""
    readsPrec _ str = [(uncurry Range (bimap read (read . tail) $ break (=='-') str), "")] -}
    -- I find this one readable, could be more elegant
    readsPrec i s = [(Range beg end, v) | (beg, t) <- readsPrec (i+1) s,
                                          ("-", u) <- lex t,
                                          (end, v) <- readsPrec (i+1) u]
    {- I don't really like this one
    readPrec = Range <$> readPrec <*> (traverse pChar "-" *> readPrec) where
        pChar c = do
            c' <- get
            if c == c'
            then return c
            else pfail
    -}
    {- Doesn't work, I don't know why
    readPrec = uncurry Range <$> readTup2

-- From GHC.Read: read a tuple without the parens
readTup2 :: (Read a, Read b) => ReadPrec (a, b)
readTup2 = do x <- readPrec
              readDash
              y <- readPrec
              return (x,y)

readComma :: ReadPrec ()
readComma = expectP (L.Punc ",")

readDash :: ReadPrec ()
readDash = expectP (L.Punc "-")
-}
