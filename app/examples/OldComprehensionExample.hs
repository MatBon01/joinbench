module Main where

import Data.Bag
import Data.Either
import Text.Parser.Customers as CP
import Text.Parser.Invoices as IP

main :: IO ()
main = do
  customersCSV <- readFile "app/examples/tables/c100.csv"
  let customers = fromRight empty $ CP.parseCSV customersCSV
  invoicesCSV <- readFile "app/examples/tables/i100.csv"
  let invoices = fromRight empty $ IP.parseCSV invoicesCSV
  let join = Bag [(c, i) | c <- elements customers, i <- elements invoices, cid c == cust i]
  print join
