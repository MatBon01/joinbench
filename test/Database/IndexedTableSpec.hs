module Database.IndexedTableSpec (spec) where

import qualified Data.Bag as Bag
import qualified Data.Key as Map
import Data.Monoid
import qualified Database.IndexedTable as Table
import Test.Hspec

type Name = String

data Person = Person {firstName :: Name, lastName :: Name} deriving (Show, Eq)

people :: Map.Map () (Bag.Bag Person)
people = Map.Lone (Bag.Bag [Person "John" "Smith", Person "Jane" "Doe", Person "John" "Doe"])

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
    describe "project" $ do
        it "can correctly do a general project" $ do
            Table.project firstName people `shouldBe` Map.Lone (Bag.Bag ["John", "Jane", "John"])
        it "can correctly project on an empty map" $ do
            Table.project lastName (Table.empty :: Map.Map () (Bag.Bag Person)) `shouldBe` Map.empty
        it "can correctly use the identity project" $ do
            Table.project id people `shouldBe` people
    describe "selection" $ do
        it "can correctly select in general" $ do
            Table.selection ((== "John") . firstName) people `shouldBe` Map.Lone (Bag.Bag [Person "John" "Smith", Person "John" "Doe"])
        it "can correctly select all elements of a table" $ do
            Table.selection (const True) people `shouldBe` people
        it "can correctly select no elements of a table" $ do
            Table.selection (const False) people `shouldBe` Map.empty
    describe "aggregation" $ do
        it "can correctly aggregate a table in general" $ do
            Table.aggregation (Map.Lone (Bag.Bag [Any True, Any True, Any False])) `shouldBe` Map.Lone (Any True)
    describe "natural join" $ do
        it "is a local cartesian product" $ do
            Table.naturalJoin (Map.Lone (Bag.Bag [1, 2]), Map.Lone (Bag.Bag [2, 3])) `shouldBe` Map.Lone (Bag.Bag [(1, 2), (1, 3), (2, 2), (2, 3)])
