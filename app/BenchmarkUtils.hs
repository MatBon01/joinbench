module BenchmarkUtils where

import Criterion.Main
import Database.Bag

equijoinBenchmark name fa fb as bs =
    bgroup
        name
        [ bench "modular product" $ whnf (productEquijoin fa fb) (as, bs)
        , bench "old comprehension" $ whnf (comprehensionEquijoin fa fb) (as, bs)
        , bench "modular indexed" $ whnf (indexedEquijoin fa fb) (as, bs)
        ]
