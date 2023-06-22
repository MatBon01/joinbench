module Main where

import           Data.Bag              as Bag
import           Data.Either
import           Database.Bag          as DB
import           Text.Parser.Customers as CP
import           Text.Parser.Invoices  as IP

main :: IO ()
main = do
  customersCSV <- readFile "app/examples/tables/c100.csv"
  let customers = fromRight Bag.empty $ CP.parseCSV customersCSV
  invoicesCSV <- readFile "app/examples/tables/i100.csv"
  let invoices = fromRight Bag.empty $ IP.parseCSV invoicesCSV
  let join = DB.indexedEquijoin cid cust (customers, invoices)
  print join
