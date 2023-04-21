module Data.PointedSet where

import Data.Maybe

class PointedSet a where
  null :: a
  isNull :: a -> Bool

instance PointedSet (Maybe a) where
  null = Nothing
  isNull = isNothing
