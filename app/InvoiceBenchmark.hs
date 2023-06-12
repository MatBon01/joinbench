module Main where

import           Criterion.Main
import           Data.Bag
import           Data.Either
import           Text.Parser.Customers as Customers
import           Text.Parser.Invoices  as Invoices


main :: IO ()
main = do
  customers <- readFile "/tmp/tables/c100.csv"
  invoices <- readFile "/tmp/tables/i100.csv"
  defaultMain [
    bgroup "parse" [ bench "customers" $ whnf Customers.parseCSV customers
                   , bench "invoices" $ whnf Invoices.parseCSV invoices ]
    ]
