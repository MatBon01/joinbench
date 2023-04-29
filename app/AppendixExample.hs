module Main where

import Data.Bag
import qualified Database.Bag as BDB

-- Define types used in example
type Identifier = String
type Name = String
type Date = String
type Amount = Float

data Customer = C { cid :: Identifier, name :: Name} deriving (Show, Eq)
data Invoice = I 
  { iid :: Identifier
  , cust :: Identifier
  , due :: Date
  , amount :: Amount} deriving (Show, Eq)

-- Cartesian product of both databases
exampleWithCP :: (Bag Customer, Bag Invoice) -> Bag (Name, Amount)
exampleWithCP = BDB.project transformation . BDB.select cond . BDB.equijoinWithCp cid cust
  where
    cond (c, i) = due i < today
    transformation (c, i) = (name c, amount i)
    today = "0"


main :: IO ()
main = do
  putStrLn "Example from appendix"
