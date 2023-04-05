module PointedSet where

class PointedSet a where
  null :: a
  isNull :: a -> Bool
