module Data.Bag where

import Data.List

newtype Bag a = Bag { elements :: [a]}
  deriving (Show)

instance (Eq a) => Eq (Bag a) where
  Bag xs == Bag ys = xs `elem` permutations ys

instance Functor Bag where
  fmap f (Bag xs) = Bag (fmap f xs)

instance Applicative Bag where
  pure x = Bag [x]
  (<*>) b1 b2 = Bag $ (<*>) (elements b1) (elements b2)

instance Monad Bag where
  Bag xs >>= k = Bag (xs >>= (elements . k))

empty :: Bag a
empty = Bag []

union :: Bag a -> Bag a -> Bag a
b1 `union` b2 = Bag (elements b1 ++ elements b2)
