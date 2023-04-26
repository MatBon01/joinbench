module Database.BagSpec (spec) where

import Test.Hspec
import qualified Database.Bag as DB
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "Database.Bag empty" $ do
    it "produces an empty Bag of the same type" $ do
      (DB.empty :: DB.Table Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
  describe "Database.Bag single" $ do
    it "produces a Bag with a single element" $ do
      DB.single 5 `shouldBe` Bag.Bag [5]
  describe "Database.Bag union" $ do
    it "uses the bag union to calculate the union" $ do
      Bag.Bag [1, 2, 3] `DB.union` Bag.Bag [3, 4, 5] `shouldBe` Bag.Bag [1, 2, 3, 3, 4, 5]
    it "can correctly deal with one empty table in the union" $ do
      Bag.Bag [1, 2, 3] `DB.union` DB.empty `shouldBe` Bag.Bag [1, 2, 3]
    it "can find the union of two empty tables" $ do
      (DB.empty :: DB.Table Char) `DB.union` DB.empty `shouldBe` (DB.empty :: DB.Table Char)
