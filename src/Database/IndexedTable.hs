module Database.IndexedTable where

import qualified Data.Bag as Bag
import qualified Data.Key as Map

empty :: (Map.Key k) => Map.Map k (Bag.Bag v)
empty = Map.empty

singleton :: (Map.Key k) => (k, v) -> Map.Map k (Bag.Bag v)
singleton (k, v) = Map.single (k, Bag.single v)

union :: (Map.Key k) => Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag v) -> Map.Map k (Bag.Bag v)
union t1 t2 = (fmap (uncurry Bag.union) . Map.merge) (t1, t2)
