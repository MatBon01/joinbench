module Data.CMonoidSpec (spec) where

import Test.Hspec
import Data.Monoid

spec :: Spec
spec = do
  describe "CMonoid instance of Sum k" $ do
    it "is commutative" $ do
      (3 :: Sum Int) <> (5 :: Sum Int) `shouldBe` (5 :: Sum Int) <> (3 :: Sum Int)

  describe "CMonoid instance of Product k" $ do
    it "is commutative" $ do
      (13 :: Product Int) <> (23 :: Product Int) `shouldBe` (23 :: Product Int) <> (13 :: Product Int)