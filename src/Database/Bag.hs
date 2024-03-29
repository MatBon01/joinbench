{-# LANGUAGE MonadComprehensions #-}

module Database.Bag where

import qualified Data.Bag as Bag
import Data.CMonoid
import Data.Key
import Database.IndexedTable

type Table = Bag.Bag

empty :: Table a
empty = Bag.empty

single :: a -> Table a
single = Bag.single

union :: (Table a, Table a) -> Table a
union = Bag.union

fromList :: [a] -> Table a
fromList = Bag.Bag

cp :: (Table a, Table b) -> Table (a, b)
cp = Bag.cp

-- TODO:: check
neutral :: Table ()
neutral = Database.Bag.single ()

project :: (a -> b) -> Table a -> Table b
project = fmap

select :: (a -> Bool) -> Table a -> Table a
select p = Bag.Bag . filter p . Bag.elements

aggregate :: (CMonoid a) => Table a -> a
aggregate = Bag.reduceBag

productEquijoin :: (Eq c) => (a -> c) -> (b -> c) -> (Table a, Table b) -> Table (a, b)
productEquijoin fa fb = select equality . cp
  where
    equality (a, b) = fa a == fb b

indexBy :: (Key k) => Table a -> (a -> k) -> Map k (Table a)
indexBy bs keyProj = (index . fmap (\x -> (keyProj x, x))) bs

indexedEquijoin :: (Key k) => (a -> k) -> (b -> k) -> (Table a, Table b) -> Table (a, b)
-- t1, t2 Bags
-- if1, if2 are indexing functions
indexedEquijoin if1 if2 (t1, t2) = (reduce . fmap cp . merge) (it1, it2)
  where
    -- Indexed table 1 and 2
    it1 = t1 `indexBy` if1
    it2 = t2 `indexBy` if2

comprehensionEquijoin :: (Eq c) => (a -> c) -> (b -> c) -> (Table a, Table b) -> Table (a, b)
comprehensionEquijoin fa fb (as, bs) = [(a, b) | a <- as, b <- bs, fa a == fb b]
