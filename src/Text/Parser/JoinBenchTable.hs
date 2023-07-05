module Text.Parser.JoinBenchTable where

import Data.Bag
import Text.Parser.Utils
import Text.ParserCombinators.Parsec

data JoinBenchRecord = B {id :: Int, onePercent :: Int, twentyPercent :: Int, twentyfivePercent :: Int, fiftyPercent :: Int, evenOnePercent :: Int, oddOnePercent :: Int}
    deriving (Show, Eq)

csvFile :: GenParser Char st (Bag JoinBenchRecord)
csvFile = do
    result <- many record
    eof
    return (Bag result)

record :: GenParser Char st JoinBenchRecord
record = do
    id <- intCell
    separator
    onePercent <- intCell
    separator
    twentyPercent <- intCell
    separator
    twentyfivePercent <- intCell
    separator
    fiftyPercent <- intCell
    separator
    evenOnePercent <- intCell
    separator
    oddOnePercent <- intCell
    eol
    return (B id onePercent twentyPercent twentyfivePercent fiftyPercent evenOnePercent oddOnePercent)

intCell :: GenParser Char st Int
intCell = do
    x <- many1 digit
    return (read x)

parseCSV :: String -> Either ParseError (Bag JoinBenchRecord)
parseCSV = parse csvFile "(unknown)"
