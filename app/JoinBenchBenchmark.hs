{-# LANGUAGE MonadComprehensions #-}

module Main where

import BenchmarkUtils
import Criterion.Main
import Data.Either
import Database.Bag
import Text.Parser.JoinBenchTable

main :: IO ()
main = do
    table <- getJoinBenchTable
    defaultMain
        [ equijoinBenchmark "join on onePercent" onePercent onePercent table table
        , equijoinBenchmark "join onePercent and twentyPercent" onePercent twentyPercent table table
        , equijoinBenchmark "join onePercent and fiftyPercent" onePercent fiftyPercent table table
        , equijoinBenchmark "join even and odd" evenOnePercent oddOnePercent table table
        ]

getJoinBenchTable :: IO (Table JoinBenchRecord)
getJoinBenchTable = do
    let joinBenchFileName = "tables/join_bench_table.csv"
    joinBenchCSV <- readFile joinBenchFileName
    return $ fromRight empty $ parseCSV joinBenchCSV
