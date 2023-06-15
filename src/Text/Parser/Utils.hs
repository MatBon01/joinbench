module Text.Parser.Utils where

import           Text.ParserCombinators.Parsec

separator :: GenParser Char st Char
separator = char ','

eol :: GenParser Char st String
eol = try (string "\r\n") <|> try (string "/n/r") <|> string "\n"

