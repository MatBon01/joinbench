module Main where

import Data.Bag as Bag
import qualified Database.Bag as BDB
import Database.IndexedTable as IT
import Data.Word
import Data.Key

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

predefinedIndexedEquijoin :: (Bag Customer, Bag Invoice) -> Bag (Name, Amount)
predefinedIndexedEquijoin
  = BDB.project exampleProjection . BDB.select exampleSelectionCond . BDB.indexedEquijoin (fromIntegral . cid :: Customer -> Word16) (fromIntegral . cust)

explicitIndexedJoin :: (Bag Customer, Bag Invoice) -> Bag (Name, Amount)
explicitIndexedJoin (customer, invoices)= reduceBag (fmap cp (example customer invoices))
    where
      pair (f, g) (a, b) = (f a, g b)
      -- Note: snd had to be added to example instead of copying from appendix
      example cs is = fmap (pair (fmap name, fmap amount)) (cod
          (fmap (pair (id, Bag.filter ((< today) . due ))) (merge (cs `BDB.indexBy` (fromIntegral . cid :: Customer -> Word16), is `BDB.indexBy` (fromIntegral . cust)))))


main :: IO ()
main = do
  putStrLn "Example from appendix"
  putStrLn "Expected result: [('sam',15),('pat',10)]"

  putStrLn "Using bags and Cartesian products:"
  print (productExample (customers, invoices))

  putStrLn "Using predefined indexed equijoin"
  print (predefinedIndexedEquijoin (customers, invoices))

  putStrLn "Using explicit indexed equijoin similar to appendix"
  print (explicitIndexedJoin (customers, invoices))
