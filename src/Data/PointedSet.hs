module Data.PointedSet where

import Data.Maybe

class PointedSet a where
    null :: a
    isNull :: a -> Bool

instance PointedSet (Maybe a) where
    null = Nothing
    isNull = isNothing

instance (PointedSet v1, PointedSet v2) => (PointedSet (v1, v2)) where
    null = (Data.PointedSet.null, Data.PointedSet.null)
    isNull (v1, v2) = isNull v1 && isNull v2
