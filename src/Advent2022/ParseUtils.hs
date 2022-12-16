{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Advent2022.ParseUtils (
    integer,
    prefixedInt,
    parseNonSpace,
    parseInt,
    getNotWhitespace,
    runReadP,
    runParsec,
    execParser
    ) where

import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Parsec
import Data.Char (isDigit, isSpace)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)

prefixedInt :: String -> ReadP.ReadP Int
prefixedInt s = ReadP.string s *> parseInt

parseInt :: ReadP.ReadP Int
parseInt = read <$> ((++) <$> optional' (ReadP.char '-') <*> ReadP.many1 (ReadP.satisfy isDigit))

optional' :: ReadP.ReadP a -> ReadP.ReadP String
optional' a = fst <$> ReadP.gather (ReadP.optional a)

getNotWhitespace :: ReadP.ReadP String
getNotWhitespace = ReadP.many1 (ReadP.satisfy (not . isSpace))

class Parser p where
    type Output p :: Type
    execParser :: p -> String -> Output p

instance Parser (ReadP.ReadP a) where
    type Output (ReadP.ReadP a) = a
    execParser = runReadP

instance Parser (ParsecT String () Identity a) where
    type Output (ParsecT String () Identity a) = a
    execParser = runParsec

runReadP :: ReadP.ReadP a -> String -> a
runReadP p = fst . last . ReadP.readP_to_S p

runParsec :: (Stream s Identity t) => Parsec s () a -> s -> a
runParsec parser s = case Text.Parsec.parse parser "" s of
    Left e -> error (show e)
    Right a -> a

-- Copied from Text.Parsec.Token: i didn't find a way to extract them from the GenTokenParser

natural :: (Read a, Integral a) => Parsec String u a
natural = read <$> many1 (satisfy isDigit)

integer :: (Read a, Integral a) => Parsec String u a
integer = sign <*> natural

sign :: (Num a) => Parsec String u (a -> a)
sign = (char '+' >> return id)
    <|> (char '-' >> return negate)
    <|> return id

parseNonSpace :: Parsec String u String
parseNonSpace = many1 (satisfy (not . isSpace))
