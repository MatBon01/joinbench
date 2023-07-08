{-# LANGUAGE ExplicitForAll #-}

module BenchmarkUtils where

import Criterion.Main
import Data.Key
import Database.Bag

equijoinBenchmark name fa fb as bs =
    bgroup
        name
        (map equijoinTransform equijoinNameAndMethod)
  where
    equijoinTransform (name, func) = bench name $ nf (func fa fb) (as, bs)

equijoinNameAndMethod :: forall k a b. (Eq k, Key k) => [(String, (a -> k) -> (b -> k) -> (Table a, Table b) -> Table (a, b))]
equijoinNameAndMethod =
    [ ("modular product", productEquijoin)
    , ("old comprehension", comprehensionEquijoin)
    , ("modular indexed", indexedEquijoin)
    ]
