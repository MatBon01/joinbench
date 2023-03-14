{-# LANGUAGE InstanceSigs #-}
module Data.Bag where

newtype Bag a = Bag { elements :: [a]}
  deriving (Eq, Show)

instance Functor Bag where
  fmap f (Bag xs) = Bag (fmap f xs)

instance Applicative Bag where
  pure x = Bag [x]
  (<*>) b1 b2 = Bag $ (<*>) (elements b1) (elements b2)