module Text.Parser.JoinBenchTableSpec where

import Data.Bag as Bag
import Data.Either
import Test.Hspec
import Text.Parser.JoinBenchTable

expectedOutput :: Bag BenchmarkRecord
expectedOutput = Bag [B 0 29 4 2 1 40 49, B 1 95 3 3 0 76 115]

spec :: Spec
spec = do
    describe "parseCSV" $ do
        it "can correctly parse the example" $ do
            testFile <- readFile "test/Text/Parser/joinbenchtest.csv"
            fromRight Bag.empty (parseCSV testFile) `shouldBe` expectedOutput
