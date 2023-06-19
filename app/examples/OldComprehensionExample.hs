module Main where

import Text.Parser.Customers as CP
import Text.Parser.Invoices as IP
import Data.Bag
import Data.Either

main :: IO ()
main = do
  customersCSV <- readFile "app/examples/tables/c100.csv"
  let customers = fromRight empty $ CP.parseCSV customersCSV
  invoicesCSV <- readFile "app/examples/tables/i100.csv"
  let invoices = fromRight empty $ IP.parseCSV invoicesCSV
  print $ head $ elements customers
  print $ head $ elements invoices

