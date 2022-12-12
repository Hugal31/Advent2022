{-# LANGUAGE FlexibleContexts #-}

module Advent2022.ParseUtils where

import Text.ParserCombinators.ReadP
import qualified Text.Parsec
import Data.Char (isDigit, isSpace)
import Data.Functor.Identity (Identity)

prefixedInt :: String -> ReadP Int
prefixedInt s = string s *> parseInt

parseInt :: ReadP Int
parseInt = read <$> ((++) <$> optional' (char '-') <*> many1 (satisfy isDigit))

optional' :: ReadP a -> ReadP String
optional' a = fst <$> gather (optional a)

getNotWhitespace :: ReadP String
getNotWhitespace = many1 (satisfy (not . isSpace))

runReadP :: ReadP a -> String -> a
runReadP p = fst . last . readP_to_S p

runParsec :: (Text.Parsec.Stream s Identity t) => Text.Parsec.Parsec s () a -> s -> a
runParsec parser s = case Text.Parsec.parse parser "" s of
  Left e -> error (show e)
  Right a -> a
