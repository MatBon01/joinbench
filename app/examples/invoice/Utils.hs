module Utils where

import Data.Either
import Database.Bag
import Text.Parser.Customers as CP
import Text.Parser.Invoices as IP

path :: String
path = "app/examples/invoice/tables/"

makeTableName :: String -> String
makeTableName name = path ++ name

loadCustomerDatabase :: IO (Table Customer)
loadCustomerDatabase = do
    customersCSV <- readFile $ makeTableName "c100.csv"
    let customers = fromRight empty $ CP.parseCSV customersCSV
    return customers

loadInvoiceDatabase :: IO (Table Invoice)
loadInvoiceDatabase = do
    invoicesCSV <- readFile $ makeTableName "i100.csv"
    let invoices = fromRight empty $ IP.parseCSV invoicesCSV
    return invoices
