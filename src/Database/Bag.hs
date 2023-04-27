module Database.Bag where

import qualified Data.Bag as Bag

type Table = Bag.Bag

empty :: Table a
empty = Bag.empty

single :: a -> Table a
single = Bag.single

union :: Table a -> Table a -> Table a
union = Bag.union

cp :: Table a -> Table b -> Table (a, b)
cp = Bag.cp

-- TODO:: check
neutral :: Table ()
neutral = single ()

projection :: (a -> b) -> Table a -> Table b
projection = fmap

selection :: (a -> Bool) -> Table a -> Table a
selection p = Bag.Bag . filter p . Bag.elements
