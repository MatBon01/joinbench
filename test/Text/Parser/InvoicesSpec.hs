module Text.Parser.InvoicesSpec where

import Data.Bag as Bag
import Data.Either
import Test.Hspec
import Text.Parser.Invoices
import Text.ParserCombinators.Parsec

expectedOutput = Bag [I 10 1 20160101 10, I 11 1 20160102 83, I 12 2 20160103 15]

spec :: Spec
spec = do
  describe "parseCSV" $ do
    it "can correctly parse the example" $ do
      testFile <- readFile "test/Text/Parser/invoicestest.csv"
      fromRight Bag.empty (parseCSV testFile) `shouldBe` expectedOutput
