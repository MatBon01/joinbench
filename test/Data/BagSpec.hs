module Data.BagSpec (spec) where

import Test.Hspec
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "Data.Bag fmap" $ do
    it "maps over the underlying list" $ do
      fmap (5 +) (Bag.Bag [0, 10, 20, 30]) `shouldBe` Bag.Bag [5, 15, 25, 35]
