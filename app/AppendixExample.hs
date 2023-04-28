module Main where

import Data.Bag

-- Define types used in example
type Identifier = String
type Name = String
type Date = String
type Amount = Float

data Customer = C { cid :: Identifier, name :: Name}
data Invoice = I { iid :: Identifier, cust :: Identifier, due :: Date, amount :: Amount}

-- Cartesian product of both databases
exampleWithCP :: Bag Customer -> Bag Invoice -> Bag (Customer, Invoice)
exampleWithCP cs is = cp cs is

main :: IO ()
main = do
  putStrLn "Example from appendix"
