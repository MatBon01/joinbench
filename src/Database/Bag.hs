module Database.Bag where

import qualified Data.Bag as Bag
import Data.CMonoid

type Table = Bag.Bag

empty :: Table a
empty = Bag.empty

single :: a -> Table a
single = Bag.single

union :: (Table a, Table a) -> Table a
union = Bag.union

cp :: Table a -> Table b -> Table (a, b)
cp = Bag.cp

-- TODO:: check
neutral :: Table ()
neutral = single ()

projection :: (a -> b) -> Table a -> Table b
projection = fmap

select :: (a -> Bool) -> Table a -> Table a
select p = Bag.Bag . filter p . Bag.elements

aggregate :: CMonoid a => Table a -> a
aggregate = Bag.reduceBag

equijoinWithCp :: Eq c => (a -> c) -> (b -> c) ->  Table a -> Table b -> Table (a, b)
equijoinWithCp fa fb as bs = select equality (cp as bs)
  where 
    equality (a, b) = fa a == fb b


