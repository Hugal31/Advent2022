module Advent2022.ParseUtils where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit, isSpace)

prefixedInt :: String -> ReadP Int
prefixedInt s = string s *> parseInt

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

getNotWhitespace :: ReadP String
getNotWhitespace = many1 (satisfy (not . isSpace))

runReadP :: ReadP a -> String -> a
runReadP p = fst . last . readP_to_S p
