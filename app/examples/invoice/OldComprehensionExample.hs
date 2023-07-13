{-# LANGUAGE MonadComprehensions #-}

module Main where

import Data.Either
import Database.Bag
import Text.Parser.Customers as CP
import Text.Parser.Invoices as IP

import Utils

main :: IO ()
main = do
    customers <- loadCustomerDatabase
    invoices <- loadInvoiceDatabase
    let join = [(c, i) | c <- customers, i <- invoices, cid c == cust i]
    print join
