{-# LANGUAGE MonadComprehensions #-}

module Main where

import BenchmarkUtils
import Criterion.Main
import Data.Either
import Database.Bag
import System.Environment
import Text.Parser.JoinBenchTable

numArgs = 1
joinBenchFileNameArgIndex = 0

main :: IO ()
main = do
    joinBenchFileName <- getJoinBenchTableName
    table <- readJoinBenchTable joinBenchFileName

    args <- getArgs
    withArgs (drop numArgs args) $
        defaultMain
            [ equijoinBenchmark "join onePercent and onePercent" onePercent onePercent table table
            , equijoinBenchmark "join onePercent and twentyPercent" onePercent twentyPercent table table
            , equijoinBenchmark "join twentyPercent and onePercent" twentyPercent onePercent table table
            , equijoinBenchmark "join onePercent and fiftyPercent" onePercent fiftyPercent table table
            , equijoinBenchmark "join evenOnePercent and oddOnePercent" evenOnePercent oddOnePercent table table
            ]

getJoinBenchTableName :: IO String
getJoinBenchTableName = do
    args <- getArgs
    return $ args !! joinBenchFileNameArgIndex
