{-# LANGUAGE InstanceSigs #-}

module Data.Bag where

import Control.Applicative
import Control.DeepSeq
import Data.CMonoid
import Data.List
import qualified Data.PointedSet as Pointed

newtype Bag a = Bag {elements :: [a]}

instance (Show a) => (Show (Bag a)) where
    show = intercalate "\n" . map show . elements

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
    (<>) = curry Data.Bag.union

instance Monoid (Bag a) where
    mempty = Data.Bag.empty

instance Alternative Bag where
    empty = Data.Bag.empty
    (<|>) = (<>)

instance CMonoid (Bag a)

instance Pointed.PointedSet (Bag a) where
    null = Data.Bag.empty
    isNull :: Bag a -> Bool
    -- Implement isNull with pattern matching to not require Eq a
    isNull (Bag []) = True
    isNull _ = False

instance (NFData a) => NFData (Bag a) where
    rnf (Bag xs) = rnf xs

empty :: Bag a
empty = Bag []

union :: (Bag a, Bag a) -> Bag a
union (b1, b2) = Bag (elements b1 ++ elements b2)

reduceBag :: (CMonoid m) => Bag m -> m
-- Reduce a bag of ms into an m (e.g. a bag of bags into a bag)
reduceBag = mconcat . elements

-- Cartesian product
cp :: (Bag a, Bag b) -> Bag (a, b)
cp (Bag xs, Bag ys) = Bag [(x, y) | x <- xs, y <- ys]

-- Create singleton bag
single :: a -> Bag a
single = pure

-- Filter
filter :: (a -> Bool) -> Bag a -> Bag a
filter p = Bag . Prelude.filter p . elements
