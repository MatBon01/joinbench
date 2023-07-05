module Main where

import Data.Either
import Database.Bag
import Text.Parser.JoinBenchTable

main :: IO ()
main = do
    table <- getJoinBenchTable
    print "table loaded"

getJoinBenchTable :: IO (Table JoinBenchRecord)
getJoinBenchTable = do
    let joinBenchFileName = "join_bench_table.csv"
    joinBenchCSV <- readFile joinBenchFileName
    return $ fromRight empty $ parseCSV joinBenchCSV
