{-# LANGUAGE MonadComprehensions #-}
module Main where

import Criterion.Main
import Data.Either
import Database.Bag
import System.Environment
import Text.Parser.Customers as Customers
import Text.Parser.Invoices as Invoices

numArgs = 2
customerArgIndex = 0
invoiceArgIndex = 1

main :: IO ()
main = do
    args <- getArgs

    customersCSV <- readFile $ args !! customerArgIndex
    invoicesCSV <- readFile $ args !! invoiceArgIndex

    let customers = fromRight empty $ Customers.parseCSV customersCSV
    let invoices = fromRight empty $ Invoices.parseCSV invoicesCSV

    withArgs (drop numArgs args) $
        defaultMain
            [ bgroup
                "parse"
                [ bench "customers" $ whnf Customers.parseCSV customersCSV
                , bench "invoices" $ whnf Invoices.parseCSV invoicesCSV
                ]
            , bgroup
                "joins"
                [ bench "modular product" $
                    whnf (productEquijoin cid cust) (customers, invoices)
                , bench "old comprehension" $
                    whnf oldComprehension (customers, invoices)
                ]
            ]

oldComprehension :: (Table Customer, Table Invoice) -> Table (Customer, Invoice)
oldComprehension (cs, is) = [ (c, i) | c <- cs, i <- is, cid c == cust i ]
