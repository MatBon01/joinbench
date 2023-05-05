module Text.Parser.Invoices where

import Text.ParserCombinators.Parsec
import Data.Bag

type Identifier = Int
type Date = Int
type Amount = Int

data Invoice = I 
  { iid :: Identifier
  , cust :: Identifier
  , due :: Date
  , amount :: Amount} deriving (Show, Eq)

csvFile :: GenParser Char st (Bag Invoice)
csvFile = do
  result <- many invoiceRecord
  eof
  return (Bag result)

invoiceRecord :: GenParser Char st Invoice
invoiceRecord = do
  iidCell <- identifier 
  separator
  custIdCell <- identifier
  separator
  dueCell <- date
  separator
  amountCell <- price
  eol
  return (I iidCell custIdCell dueCell amountCell)

identifier :: GenParser Char st Identifier
identifier = do
  id <- many digit
  return (read id)

price :: GenParser Char st Amount
price = do
  id <- many digit
  return (read id)

date :: GenParser Char st Date
date = do
  id <- many digit
  return (read id)

separator :: GenParser Char st Char
separator = char ','

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError (Bag Invoice)
parseCSV input = parse csvFile "(unknown)" input
