module Main where

import Database.Bag
import Text.Parser.JoinBenchTable

strictPair (a, b) = a `seq` b `seq` (a, b)

main = do
    joinbench <- getJoinBenchTable "tables/join_bench_table_5000.csv"

    let joinedOnePercentWithFiftyPercent = indexedEquijoin onePercent fiftyPercent (joinbench, joinbench)

    print joinedOnePercentWithFiftyPercent
