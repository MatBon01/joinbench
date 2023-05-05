module Text.Parser.Customers where

import Text.ParserCombinators.Parsec
import Data.Bag

type Identifier = Int
type Name = String
data Customer = C { cid :: Identifier, name :: Name } deriving (Show, Eq)

csvFile :: GenParser Char st (Bag Customer)
csvFile = do
  result <- many record
  eof
  return (Bag result)

record :: GenParser Char st Customer
record = do
  id <- cidCell
  separator
  name <- nameCell
  eol
  return (C id name)

cidCell :: GenParser Char st Identifier
cidCell = do
  id <- many digit
  return (read id)

nameCell :: GenParser Char st Name
nameCell = many (noneOf ",\n")

separator :: GenParser Char st Char
separator = char ','

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError (Bag Customer)
parseCSV input = parse csvFile "(unknown)" input