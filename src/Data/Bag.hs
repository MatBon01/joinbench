{-# LANGUAGE InstanceSigs #-}
module Data.Bag where

import Data.List
import Data.CMonoid
import qualified Data.PointedSet as Pointed

newtype Bag a = Bag {elements :: [a]}
  deriving (Show)

instance (Eq a) => Eq (Bag a) where
  b1 == b2 = eq' (elements b1) (elements b2)
    where
      eq' :: (Eq a) => [a] -> [a] -> Bool
      eq' (x : xs) ys = not (null ys2) && eq' xs (ys1 ++ tail ys2)
        where
          (ys1, ys2) = break (== x) ys
      eq' [] [] = True
      eq' _ _ = False

instance Functor Bag where
  fmap f (Bag xs) = Bag (fmap f xs)

instance Applicative Bag where
  pure x = Bag [x]
  (<*>) b1 b2 = Bag $ (<*>) (elements b1) (elements b2)

instance Monad Bag where
  Bag xs >>= k = Bag (xs >>= (elements . k))

instance Semigroup (Bag a) where
  (<>) = Data.Bag.union

instance Monoid (Bag a) where
  mempty = Data.Bag.empty

instance CMonoid (Bag a)

instance Pointed.PointedSet (Bag a) where
  null = empty
  isNull :: Bag a -> Bool
  -- Implement isNull with pattern matching to not require Eq a
  isNull (Bag []) = True
  isNull _ = False

empty :: Bag a
empty = Bag []

union :: Bag a -> Bag a -> Bag a
b1 `union` b2 = Bag (elements b1 ++ elements b2)

reduce :: CMonoid m => Bag m -> m
reduce = mconcat . elements