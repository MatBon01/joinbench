{-# LANGUAGE MonadComprehensions #-}

module Main where

import Criterion.Main
import Data.Either
import Database.Bag
import Text.Parser.JoinBenchTable

main :: IO ()
main = do
    table <- getJoinBenchTable

    defaultMain
        [ bgroup
            "join on onePercent"
            [ bench "modular product" $ whnf (productEquijoin onePercent onePercent) (table, table)
            , bench "old comprehension" $ whnf (comprehensionEquijoin onePercent onePercent) (table, table)
            , bench "modular indexed" $ whnf (indexedEquijoin onePercent onePercent) (table, table)
            ]
        ]

getJoinBenchTable :: IO (Table JoinBenchRecord)
getJoinBenchTable = do
    let joinBenchFileName = "join_bench_table.csv"
    joinBenchCSV <- readFile joinBenchFileName
    return $ fromRight empty $ parseCSV joinBenchCSV
