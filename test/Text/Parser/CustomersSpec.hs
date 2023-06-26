module Text.Parser.CustomersSpec where

import Data.Bag as Bag
import Data.Either
import Test.Hspec
import Text.Parser.Customers
import Text.ParserCombinators.Parsec

expectedOutput = Bag [C 1 "John" "Williams", C 2 "Kayla" "Johnson"]

spec :: Spec
spec = do
    describe "parseCSV" $ do
        it "can correctly parse the example" $ do
            testFile <- readFile "test/Text/Parser/customertest.csv"
            fromRight Bag.empty (parseCSV testFile) `shouldBe` expectedOutput
