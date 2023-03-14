module Data.BagSpec (spec) where

import Test.Hspec
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "Data.Bags.singleton" $ do
    it "creates a bag with only the element given" $ do
      (Bag.singleton 1) `shouldBe` (Bag.Bag [1])
