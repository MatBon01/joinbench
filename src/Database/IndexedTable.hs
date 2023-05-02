module Database.IndexedTable where

import qualified Data.Bag as Bag
import qualified Data.Key as Map
import Data.CMonoid

empty :: (Map.Key k) => Map.Map k (Bag.Bag v)
empty = Map.empty

singleton :: (Map.Key k) => (k, v) -> Map.Map k (Bag.Bag v)
singleton (k, v) = Map.single (k, Bag.single v)

union :: (Map.Key k) => Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag v)
union t1 t2 = (fmap Bag.union . Map.merge) (t1, t2)

project :: (Map.Key k) => (v -> w) -> Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag w)
project = fmap . fmap

selection :: (Map.Key k) => (v -> Bool) -> Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag v)
selection p = fmap (Bag.filter p)

aggregation :: (Map.Key k, CMonoid m) => Map.Map k (Bag.Bag m) -> Map.Map k m
aggregation = fmap Bag.reduceBag

-- Joins on common keys
naturalJoin :: (Map.Key k) => (Map.Map k (Bag.Bag v), Map.Map k (Bag.Bag w)) -> Map.Map k (Bag.Bag (v, w))
naturalJoin = fmap Bag.cp . Map.merge
