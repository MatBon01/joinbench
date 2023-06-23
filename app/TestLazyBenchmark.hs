module Main where

import Criterion.Main
import Data.Bag
import Data.Either
import Text.Parser.Customers as Customers
import Text.Parser.Invoices as Invoices

main :: IO ()
main = do
    customers <- readFile "/tmp/tables/c100.csv"
    invoices <- readFile "/tmp/tables/i100.csv"
    defaultMain
        [ bgroup
            "parse customers"
            [ bench "1" $ whnf Customers.parseCSV customers
            , bench "2" $ whnf Customers.parseCSV customers
            , bench "3" $ whnf Customers.parseCSV customers
            , bench "4" $ whnf Customers.parseCSV customers
            ]
        , bgroup
            "parse invoices"
            [ bench "1" $ whnf Invoices.parseCSV invoices
            , bench "2" $ whnf Invoices.parseCSV invoices
            , bench "3" $ whnf Invoices.parseCSV invoices
            , bench "4" $ whnf Invoices.parseCSV invoices
            ]
        ]
