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

