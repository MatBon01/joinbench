module Main where

import Database.Bag
import Text.Parser.JoinBenchTable

main = do
    joinbench <- getJoinBenchTable "tables/join_bench_table_5000.csv"
    let joinOnePercentWithFiftyPercent = productEquijoin onePercent fiftyPercent (joinbench, joinbench)
    print joinOnePercentWithFiftyPercent
