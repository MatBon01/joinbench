module Text.Parser.CustomersSpec where

import Test.Hspec 
import Text.Parser.Customers
import Data.Bag as Bag
import Text.ParserCombinators.Parsec
import Data.Either

expectedOutput = Bag [C 1 "John", C 2 "Kayla"]

spec :: Spec
spec = do
  describe "parseCSV" $ do
    it "can correctly parse the example" $ do
      testFile <- readFile "test/Text/Parser/customertest.csv"
      fromRight Bag.empty (parseCSV testFile) `shouldBe` expectedOutput
