module Database.IndexedTableSpec (spec) where

import Test.Hspec
import qualified Database.IndexedTable as Table
import qualified Data.Key as Map
import Data.Bag (Bag)

spec :: Spec
spec = do
  describe "empty" $ do
    it "returns an empty map" $ do
      (Table.empty :: Map.Map () (Bag Int)) `shouldBe` (Map.empty :: Map.Map () (Bag Int))
