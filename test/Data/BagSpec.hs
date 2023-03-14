module Data.BagSpec (spec) where

import Test.Hspec
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "test" $ do
    it "acts as a placeholder" $ do
      5 `shouldBe` 5
