module Data.CMonoid where

import Data.Monoid

class Monoid a => CMonoid a
-- (<>) is commutative

instance Num k => CMonoid (Sum k)
instance Num k => CMonoid (Product k)
instance CMonoid All
instance CMonoid Any
