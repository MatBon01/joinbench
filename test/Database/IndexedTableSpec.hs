module Database.IndexedTableSpec (spec) where

import Test.Hspec
import qualified Database.IndexedTable as Table
import qualified Data.Key as Map
import qualified Data.Bag as Bag

spec :: Spec
spec = do
  describe "empty" $ do
    it "returns an empty map" $ do
      (Table.empty :: Map.Map () (Bag.Bag Int)) `shouldBe` (Map.empty :: Map.Map () (Bag.Bag Int))
  describe "singleton" $ do
    it "returns a single table" $ do
      Table.singleton ((), 3) `shouldBe` Map.Lone (Bag.Bag [3])
  describe "union" $ do
    it "can correctly handle union of singletons" $ do
      Table.union (Table.singleton ((), 3)) (Table.singleton ((), 4)) `shouldBe` Map.Lone (Bag.Bag [3, 4])
    it "can correctly deal with first element empty" $ do
      Table.union (Table.empty :: Map.Map () (Bag.Bag Char)) (Table.singleton ((), 'a')) `shouldBe` Map.Lone (Bag.Bag ['a'])
    it "can correctly deal with second element empty" $ do
      Table.union (Table.singleton ((), 'a')) (Table.empty :: Map.Map () (Bag.Bag Char)) `shouldBe` Map.Lone (Bag.Bag ['a'])
