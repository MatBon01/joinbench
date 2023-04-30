{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Key where

import Data.PointedSet
import Data.Bag
import Data.CMonoid
import Data.Word
import Data.Array
import qualified Data.Bifunctor as Bifunctor

class (Functor (Map k)) => Key k where
  data Map k :: * -> *
  empty :: (PointedSet v) => Map k v
  isEmpty :: (PointedSet v) => Map k v -> Bool
  single :: (PointedSet v) => (k, v) -> Map k v
  merge :: (Map k v1, Map k v2) -> Map k (v1, v2)
  unmerge :: Map k (v1, v2) -> (Map k v1, Map k v2)
  unmerge x = (fmap fst x, fmap snd x)
  dom :: (PointedSet v) => Map k v -> Bag k
  cod :: (PointedSet v) => Map k v -> Bag v
  cod t = reduce (fmap return t)
  lookup :: Map k v -> (k -> v)
  index :: Bag (k, v) -> Map k (Bag v)
  unindex :: Map k (Bag v) -> Bag (k, v)
  reduce :: (PointedSet v, CMonoid v) => Map k v -> v
  reduce = reduceBag . cod

instance Key () where -- unit type
  newtype Map () v = Lone v deriving (Show, Eq)

  empty = Lone Data.PointedSet.null
  isEmpty (Lone v) = isNull v
  single ((), v) = Lone v
  merge (Lone v1, Lone v2) = Lone (v1, v2)
  dom map = if isEmpty map then Data.Bag.empty else pure ()
  cod (Lone v) = if isEmpty (Lone v) then Data.Bag.empty else pure v 
  lookup (Lone v) () = v
  index kvps = Lone (fmap snd kvps)
  unindex (Lone vs) = fmap (\v -> ((), v)) vs

instance Functor (Map ()) where
  fmap f (Lone v) = Lone (f v)

instance Key Word16 where -- constant type (array indexed by 16 bit word)
  newtype Map Word16 v = A (Array Word16 v) deriving (Eq, Show)
  empty = A (accumArray (curry snd) Data.PointedSet.null (0, 2^16-1) [])
  isEmpty (A a) = all isNull (elems a)
  single (k, v) = A (accumArray (curry snd) Data.PointedSet.null (0, 2^16-1) [(k, v)])
  merge (A a1, A a2) = A (listArray (0, 2^16 - 1) (zip (elems a1) (elems a2)))
  dom (A a) = Bag [ k | (k, v) <- assocs a, not (isNull v) ]
  cod (A a) = Bag [ v | (k, v) <- assocs a, not (isNull v) ]
  lookup (A a) = (!) a
  -- index :: Bag (k, v) -> Map k (Bag v)
  index kvps = A (accumArray (curry Data.Bag.union) Data.Bag.empty (0, 2^16-1) vals)
    where
      vals = (elements . fmap (Bifunctor.second Data.Bag.single)) kvps
  -- unindex :: Map k (Bag v) -> Bag (k, v)
  -- reduce :: (PointedSet v, CMonoid v) => Map k v -> v
  -- reduce = reduceBag . cod

instance Functor (Map Word16) where
  fmap f (A a) = A (fmap f a)
