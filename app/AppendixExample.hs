module Main where

import Data.Bag
import qualified Database.Bag as BDB

-- Define types used in example
type Identifier = String
type Name = String
type Date = Int
type Amount = Float

data Customer = C { cid :: Identifier, name :: Name} deriving (Show, Eq)
data Invoice = I 
  { iid :: Identifier
  , cust :: Identifier
  , due :: Date
  , amount :: Amount} deriving (Show, Eq)

today :: Date
today = 20160919

-- Cartesian product of both databases
exampleWithCP :: (Bag Customer, Bag Invoice) -> Bag (Name, Amount)
exampleWithCP = BDB.project transformation . BDB.select cond . BDB.equijoinWithCp cid cust
  where
    cond :: (Customer, Invoice) -> Bool
    cond (c, i) = due i < today
    transformation :: (Customer, Invoice) -> (Name, Amount)
    transformation (c, i) = (name c, amount i)


main :: IO ()
main = do
  putStrLn "Example from appendix"
