module Data.Bag where

newtype Bag a = Bag { elements :: [a]}
  deriving (Eq, Show)