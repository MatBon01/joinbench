module Data.Bag where

newtype Bag a = Bag { elements :: [a]}
  deriving (Eq, Show)

instance Functor Bag where
  fmap f (Bag xs) = Bag (fmap f xs)