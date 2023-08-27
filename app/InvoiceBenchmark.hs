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
                "join"
                [ bench "modular product" $
                    whnf (productEquijoin cid cust) (customers, invoices)
                , bench "old comprehension" $
                    whnf (comprehensionEquijoin cid cust) (customers, invoices)
                , bench "modular indexed" $
                    whnf (indexedEquijoin cid cust) (customers, invoices)
                ]
            , bgroup
                "join with selection"
                [ bench "modular product" $
                    whnf
                        (select selectionCriteria . productEquijoin cid cust)
                        (customers, invoices)
                , bench "old comprehension" $
                    whnf oldSelectionComprehension (customers, invoices)
                , bench "modular indexed" $
                    whnf
                        (select selectionCriteria . indexedEquijoin cid cust)
                        (customers, invoices)
                ]
            ]

selectionCriteriaSplittingPrice = 500

selectionCriteria :: (Customer, Invoice) -> Bool
selectionCriteria = (> selectionCriteriaSplittingPrice) . amount . snd

oldSelectionComprehension :: (Table Customer, Table Invoice) -> Table (Customer, Invoice)
oldSelectionComprehension (cs, is) = [(c, i) | c <- cs, i <- is, cid c == cust i, amount i > selectionCriteriaSplittingPrice]
