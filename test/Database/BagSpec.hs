module Database.BagSpec (spec) where

import Test.Hspec
import qualified Database.Bag as DB
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "Database.Bag empty" $ do
    it "produces an empty Bag of the same type" $ do
      (DB.empty :: DB.Table Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
