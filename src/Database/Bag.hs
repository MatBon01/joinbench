module Database.Bag where

import qualified Data.Bag as Bag
import Database.IndexedTable
import Data.CMonoid
import Data.Key

type Table = Bag.Bag

empty :: Table a
empty = Bag.empty

single :: a -> Table a
single = Bag.single

union :: (Table a, Table a) -> Table a
union = Bag.union

cp :: (Table a, Table b) -> Table (a, b)
cp = Bag.cp

-- TODO:: check
neutral :: Table ()
neutral = Database.Bag.single ()

project :: (a -> b) -> Table a -> Table b
project = fmap

select :: (a -> Bool) -> Table a -> Table a
select p = Bag.Bag . filter p . Bag.elements

aggregate :: CMonoid a => Table a -> a
aggregate = Bag.reduceBag

productEquijoin :: Eq c => (a -> c) -> (b -> c) ->  (Table a, Table b) -> Table (a, b)
productEquijoin fa fb = select equality . cp
  where 
    equality (a, b) = fa a == fb b

indexBy :: (Key k) => (a -> k) -> Table a -> Map k (Table a)
indexBy keyProj = index . fmap (\x -> (keyProj x, x))

indexedEquijoin :: (Key k) => (a -> k) -> (b -> k) -> (Table a, Table b) -> Table (a, b)
-- t1, t2 Bags
-- if1, if2 are indexing functions
indexedEquijoin if1 if2 (t1, t2) = (reduce . fmap cp . merge) (it1, it2)
  where
    -- Indexed table 1 and 2
    it1 = indexBy if1 t1
    it2 = indexBy if2 t2
