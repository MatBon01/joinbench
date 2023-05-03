module Main where

import Data.Bag
import qualified Database.Bag as BDB

-- Define types used in example
type Identifier = Int
type Name = String
type Date = Int
type Amount = Int

data Customer = C { cid :: Identifier, name :: Name} deriving (Show, Eq)
data Invoice = I 
  { iid :: Identifier
  , cust :: Identifier
  , due :: Date
  , amount :: Amount} deriving (Show, Eq)

today :: Date
today = 20160919

customers :: Bag Customer
customers = Bag 
  [ C 101 "sam"
  , C 102 "max"
  , C 103 "pat" ]

invoices :: Bag Invoice
invoices = Bag
  [ I 201 101 20160921 20
  , I 202 101 20160316 15
  , I 203 103 20160520 10 ]

exampleSelectionCond :: (Customer, Invoice) -> Bool
exampleSelectionCond (c, i) = due i < today

exampleProjection :: (Customer, Invoice) -> (Name, Amount)
exampleProjection (c, i) = (name c, amount i)

-- Cartesian product of both databases
productExample :: (Bag Customer, Bag Invoice) -> Bag (Name, Amount)
productExample = BDB.project exampleProjection . BDB.select exampleSelectionCond . BDB.productEquijoin cid cust

main :: IO ()
main = do
  putStrLn "Example from appendix"
  putStrLn "Expected result: [('sam',15),('pat',10)]"

  putStrLn "Using bags and Cartesian products:"
  print (productExample (customers, invoices))
