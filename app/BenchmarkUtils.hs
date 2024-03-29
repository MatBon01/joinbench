{-# LANGUAGE ExplicitForAll #-}

module BenchmarkUtils where

import Criterion.Main
import Data.Key
import Database.Bag

generalEquijoinBenchmark name fa fb as bs evalmethod =
    bgroup
        name
        (map equijoinTransform equijoinNameAndMethod)
  where
    equijoinTransform (name, func) = bench name $ evalmethod (func fa fb) (as, bs)

equijoinBenchmark name fa fb as bs =
    generalEquijoinBenchmark name fa fb as bs nf

whnfEquijoinBenchmark name fa fb as bs =
    generalEquijoinBenchmark name fa fb as bs whnf

equijoinNameAndMethod :: forall k a b. (Eq k, Key k) => [(String, (a -> k) -> (b -> k) -> (Table a, Table b) -> Table (a, b))]
equijoinNameAndMethod =
    [ ("Product equijoin", productEquijoin)
    , ("Comprehension equijoin", comprehensionEquijoin)
    , ("Indexed equijoin", indexedEquijoin)
    ]
