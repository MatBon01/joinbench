module Data.PointedSetSpec (spec) where

import Test.Hspec
import qualified Data.PointedSet as Pointed

spec :: Spec
spec = do
  describe "Maybe PointedSet" $ do
    it "has Nothing as its null value" $ do
      (Pointed.null :: Maybe Int) `shouldBe` Nothing
    it "correctly detects Nothing as null" $ do
      Pointed.isNull (Nothing :: Maybe Int) `shouldBe` True
    it "does not interpret Just as Null" $ do
      Pointed.isNull (Just 3) `shouldBe` False
