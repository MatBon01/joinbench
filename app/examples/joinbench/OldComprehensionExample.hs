module Main where

import Database.Bag
import Text.Parser.JoinBenchTable

main = do
    joinbench <- readJoinBenchTable "tables/join_bench_table_5000.csv"
    let joinOnePercentWithFiftyPercent = comprehensionEquijoin onePercent fiftyPercent (joinbench, joinbench)
    print joinOnePercentWithFiftyPercent
