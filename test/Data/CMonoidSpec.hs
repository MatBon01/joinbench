module Data.CMonoidSpec (spec) where

import Data.Monoid
import Test.Hspec

spec :: Spec
spec = do
  describe "CMonoid instance of Sum k" $ do
    it "is commutative" $ do
      (3 :: Sum Int) <> (5 :: Sum Int) `shouldBe` (5 :: Sum Int) <> (3 :: Sum Int)

  describe "CMonoid instance of Product k" $ do
    it "is commutative" $ do
      (13 :: Product Int) <> (23 :: Product Int) `shouldBe` (23 :: Product Int) <> (13 :: Product Int)
  describe "CMonoid instance of Any" $ do
    it "is commutative" $ do
      Any True <> Any False `shouldBe` Any False <> Any True
  describe "CMonoid instance of All" $ do
    it "is commutative" $ do
      All True <> All False `shouldBe` All False <> All True
