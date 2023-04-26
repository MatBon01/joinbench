module Main where

-- Define types used in example
type Identifier = String
type Name = String
type Date = String
type Amount = Float

data Customer = C { cid :: Identifier, name :: Name}
data Invoice = I { iid :: Identifier, cust :: Identifier, due :: Date, amount :: Amount}

main :: IO ()
main = do
  putStrLn "Example from appendix"
