module Text.Parser.Customers where

import Text.Parser.Utils
import           Data.Bag
import           Text.ParserCombinators.Parsec

type Identifier = Int
type Name = String
data Customer = C { cid :: Identifier, firstName :: Name, surname :: Name} deriving (Show, Eq)

csvFile :: GenParser Char st (Bag Customer)
csvFile = do
  result <- many record
  eof
  return (Bag result)

record :: GenParser Char st Customer
record = do
  firstName <- nameCell
  separator
  surname <- nameCell
  separator
  id <- cidCell
  eol
  return (C id firstName surname)

cidCell :: GenParser Char st Identifier
cidCell = do
  id <- many digit
  return (read id)

nameCell :: GenParser Char st Name
nameCell = many (noneOf ",\n")

parseCSV :: String -> Either ParseError (Bag Customer)
parseCSV input = parse csvFile "(unknown)" input
