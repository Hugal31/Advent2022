{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}

module Advent2022.Day (Day(Day), ParsedSolveFunc, makeDay, makeDayComplex, getPart) where

type ParsedSolveFunc = (String -> String)
data Day = Day ParsedSolveFunc ParsedSolveFunc

makeDayComplex :: (ToString b) => (ToString d) => (String -> a) -> (a -> b) -> (String -> c) -> (c -> d) -> Day
makeDayComplex parse1 solve1 parse2 solve2 = Day (toString . solve1 . parse1) (toString . solve2 . parse2)

makeDay :: (ToString b, ToString c) => (String -> a) -> (a -> b) -> (a -> c) -> Day
makeDay parse solve1 = makeDayComplex parse solve1 parse

getPart :: Day -> Int -> (String -> String)
getPart (Day p _) 1 = p
getPart (Day _ p) 2 = p
getPart _ _ = error "Invalid part"

class ToString a where
    toString :: a -> String

instance {-# OVERLAPPING #-} ToString String where
    toString = id

instance Show a => ToString a where
    toString = show
