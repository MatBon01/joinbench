module Data.BagSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "works" $ do
      5 `shouldBe` 5