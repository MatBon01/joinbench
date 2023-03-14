module Data.Bag where

data Bag a = Bag [a]
  deriving (Show, Eq)

empty :: Bag a
empty = Bag []

singleton :: a -> Bag a
singleton x = Bag [x]