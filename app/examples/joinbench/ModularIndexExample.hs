module Main where

import Text.Parser.JoinBenchTable

main = do
    joinbench <- getJoinBenchTable "tables/join_bench_table_100.csv"
    print joinbench
