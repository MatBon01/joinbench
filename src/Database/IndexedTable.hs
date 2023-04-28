module Database.IndexedTable where

import Data.Bag (Bag)
import Data.Key

empty :: (Key k) => Map k (Bag v)
empty = Data.Key.empty
