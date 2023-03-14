module Data.BagSpec (spec) where

import Test.Hspec
import qualified Data.Bag as Bag

import Control.Applicative

(.*) = liftA2 (*)
(.+) = liftA2 (+)

spec :: Spec
spec = do
  describe "Data.Bag fmap" $ do
    it "maps over the underlying list" $ do
      fmap (5 +) (Bag.Bag [0, 10, 20, 30]) `shouldBe` Bag.Bag [5, 15, 25, 35]
  
  describe "Data.Bag pure" $ do
    it "raises the value to a Bag whose elements is a singleton list of the value" $ do
      (pure [23, 34] :: Bag.Bag [Int]) `shouldBe` Bag.Bag [[23, 34]]
  
  describe "Data.Bag <*>" $ do
    it "should as implemented in lists" $ do
      (Bag.Bag [4,5] .* pure 2) .+ Bag.Bag [6,1] `shouldBe` Bag.Bag [14, 9, 16, 11]

