module Data.PointedSetSpec (spec) where

import qualified Data.PointedSet as Pointed
import Test.Hspec

spec :: Spec
spec = do
  describe "Maybe PointedSet" $ do
    it "has Nothing as its null value" $ do
      (Pointed.null :: Maybe Int) `shouldBe` Nothing
    it "correctly detects Nothing as null" $ do
      Pointed.isNull (Nothing :: Maybe Int) `shouldBe` True
    it "does not interpret Just as Null" $ do
      Pointed.isNull (Just 3) `shouldBe` False
  describe "Product PointedSet" $ do
    it "has (null, null) as its null value" $ do
      (Pointed.null :: (Maybe Int, Maybe Char)) `shouldBe` (Nothing, Nothing)
    it "detects (null, null) as null" $ do
      Pointed.isNull ((Nothing :: Maybe Int), (Nothing :: Maybe Char)) `shouldBe` True
    it "does not detect a pair as null if its first element is not null" $ do
      Pointed.isNull ((Just 3), (Nothing :: Maybe Char)) `shouldBe` False
    it "does not detect a pair as null if its second element is not null" $ do
      Pointed.isNull ((Nothing :: Maybe Int), (Just 'a')) `shouldBe` False
