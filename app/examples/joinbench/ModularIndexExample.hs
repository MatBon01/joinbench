module Main where

import Database.Bag
import Text.Parser.JoinBenchTable

main = do
    joinbench <- getJoinBenchTable "tables/join_bench_table_100.csv"

    let joinedOnePercentWithFiftyPercent = indexedEquijoin onePercent fiftyPercent (joinbench, joinbench)

    print joinedOnePercentWithFiftyPercent
