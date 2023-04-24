module Data.KeySpec (spec) where

import Test.Hspec
import Data.Key
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "Data.Key ()" $ do
    it "returns Lone null for empty" $ do
      (empty :: Map () (Maybe Int)) `shouldBe` Lone Nothing
    it "can detect empty" $ do
      isEmpty (Lone Nothing :: Map () (Maybe Char)) `shouldBe` True
    it "does not incorrectly detect empty" $ do
      isEmpty (Lone (Just 'a') :: Map () (Maybe Char)) `shouldBe` False
    it "returns the correct single element for not null" $ do
      single ((), Just 3) `shouldBe` Lone (Just 3)
    it "returns the correct single element for null" $ do
      single ((), Nothing :: Maybe Int) `shouldBe` Lone (Nothing :: Maybe Int)
    it "can correctly merge two maps" $ do
      merge (Lone (Just 'b'), Lone (Just 'c')) `shouldBe` Lone (Just 'b', Just 'c')
    it "can correctly unmerge two maps" $ do
      unmerge (Lone (Just 3, Just 12)) `shouldBe` (Lone (Just 3), Lone (Just 12))
    it "can correctly identify the domain of a non-empty map" $ do
      dom (Lone (Just 'e')) `shouldBe` Bag.Bag [()]
    it "can correctly identify the domain of an empty map" $ do
      dom (Lone (Nothing :: Maybe Int)) `shouldBe` Bag.empty
    it "can correctly identify the codomain of a non-empty map" $ do
      cod (Lone (Just 'e')) `shouldBe` Bag.Bag [Just 'e']
    it "can correclty identify the codomain of an empty map" $ do
      cod (Lone (Nothing :: Maybe Bool)) `shouldBe` Bag.empty
    it "can lookup elements" $ do
      Data.Key.lookup (Lone (Just True)) () `shouldBe` Just True
    it "can index a bag of pairs without multiplicty of keys" $ do
      index (Bag.Bag [((), Just 'r')]) `shouldBe` Lone (Bag.Bag [Just 'r'])
    it "can index an empty bag" $ do
      index (Bag.empty :: Bag.Bag ((), Maybe Int)) `shouldBe` Lone (Bag.Bag []) -- TODO:: Understand this
    it "can index a bag of pairs with multiplicity" $ do
      index (Bag.Bag [((), Just 'a'), ((), Just 'b'), ((), Just 'c')]) `shouldBe` Lone (Bag.Bag [Just 'a', Just 'b', Just 'c'])
