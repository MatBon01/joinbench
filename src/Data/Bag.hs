module Data.Bag where

data Bag a = Empty | Bag [a]

empty :: Bag a
empty = Empty