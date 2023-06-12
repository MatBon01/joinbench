module Main where

import Text.Parser.Customers as Customers
import Text.Parser.Invoices as Invoices
import Data.Either
import Data.Bag


main :: IO ()
main = do
  customers <- readFile "/tmp/tables/c100.csv"
  print $ head $ elements $ fromRight empty $ Customers.parseCSV customers
