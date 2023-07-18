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
            [ whnfEquijoinBenchmark "join on onePercent" onePercent onePercent table table
            , whnfEquijoinBenchmark "join onePercent and twentyPercent" onePercent twentyPercent table table
            , whnfEquijoinBenchmark "join onePercent and fiftyPercent" onePercent fiftyPercent table table
            , whnfEquijoinBenchmark "join even and odd" evenOnePercent oddOnePercent table table
            ]

getJoinBenchTableName :: IO String
getJoinBenchTableName = do
    args <- getArgs
    return $ args !! joinBenchFileNameArgIndex
