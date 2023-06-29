module Main where

import Criterion.Main
import Data.Bag
import Data.Either
import System.Environment
import Text.Parser.Customers as Customers
import Text.Parser.Invoices as Invoices

customerArgIndex = 0
invoiceArgIndex = 1

main :: IO ()
main = do
    args <- getArgs
    customers <- readFile $ args !! customerArgIndex
    invoices <- readFile $ args !! invoiceArgIndex
    defaultMain
        [ bgroup
            "parse"
            [ bench "customers" $ whnf Customers.parseCSV customers
            , bench "invoices" $ whnf Invoices.parseCSV invoices
            ]
        ]
