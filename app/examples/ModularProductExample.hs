module Main where

import Text.Parser.Customers as CP
import Text.Parser.Invoices as IP
import Data.Bag as Bag
import Database.Bag as DBag
import Data.Either


main :: IO ()
main = do
  customersCSV <- readFile "app/examples/tables/c100.csv"
  let customers = fromRight Bag.empty $ CP.parseCSV customersCSV
  invoicesCSV <- readFile "app/examples/tables/i100.csv"
  let invoices = fromRight Bag.empty $ IP.parseCSV invoicesCSV
  let join = DBag.productEquijoin cid cust (customers, invoices) 
  print join

