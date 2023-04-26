module Database.Bag where

import qualified Data.Bag as Bag

type Table = Bag.Bag

empty :: Table a
empty = Bag.empty

single :: a -> Table a
single = Bag.single

union :: Table a -> Table a -> Table a
union = Bag.union
