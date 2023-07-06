module Text.Parser.JoinBenchTable where

import Data.Bag
import Data.Word
import Text.Parser.Utils
import Text.ParserCombinators.Parsec

data JoinBenchRecord = B {id :: Int, onePercent :: Word16, twentyPercent :: Word16, twentyfivePercent :: Word16, fiftyPercent :: Word16, evenOnePercent :: Word16, oddOnePercent :: Word16}
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
    onePercent <- word16Cell
    separator
    twentyPercent <- word16Cell
    separator
    twentyfivePercent <- word16Cell
    separator
    fiftyPercent <- word16Cell
    separator
    evenOnePercent <- word16Cell
    separator
    oddOnePercent <- word16Cell
    eol
    return (B id onePercent twentyPercent twentyfivePercent fiftyPercent evenOnePercent oddOnePercent)

intCell :: GenParser Char st Int
intCell = do
    x <- many1 digit
    return (read x)

word16Cell :: GenParser Char st Word16
word16Cell = do
    x <- many1 digit
    return (read x)

parseCSV :: String -> Either ParseError (Bag JoinBenchRecord)
parseCSV = parse csvFile "(unknown)"
