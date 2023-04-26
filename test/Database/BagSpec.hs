module Database.BagSpec (spec) where

import Test.Hspec
import qualified Database.Bag as DB
import qualified Data.Bag as Bag

type Name = String
data Person = Person {firstName :: Name, lastName :: Name} deriving (Show, Eq)

people = Bag.Bag [Person "John" "Doe", Person "Jane" "Doe", Person "John" "Smith"]

spec :: Spec
spec = do
  describe "Database.Bag empty" $ do
    it "produces an empty Bag of the same type" $ do
      (DB.empty :: DB.Table Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
  describe "Database.Bag single" $ do
    it "produces a Bag with a single element" $ do
      DB.single 5 `shouldBe` Bag.Bag [5]
  describe "Database.Bag union" $ do
    it "uses the bag union to calculate the union" $ do
      Bag.Bag [1, 2, 3] `DB.union` Bag.Bag [3, 4, 5] `shouldBe` Bag.Bag [1, 2, 3, 3, 4, 5]
    it "can correctly deal with one empty table in the union" $ do
      Bag.Bag [1, 2, 3] `DB.union` DB.empty `shouldBe` Bag.Bag [1, 2, 3]
    it "can find the union of two empty tables" $ do
      (DB.empty :: DB.Table Char) `DB.union` DB.empty `shouldBe` (DB.empty :: DB.Table Char)
  describe "Database.Bag cp" $ do
    it "correctly can calculate the cartesian product of two tables" $ do
      Bag.Bag [1, 2] `DB.cp` Bag.Bag [3, 4] `shouldBe` Bag.Bag [(1, 3), (1, 4), (2, 3), (2, 4)]
    it "returns an empty table when one table is empty" $ do
      Bag.Bag [1, 2] `DB.cp` (DB.empty :: DB.Table Char) `shouldBe` (DB.empty :: DB.Table (Int, Char))
    it "returns an empty table when both tables are empty" $ do
      (DB.empty :: DB.Table Bool) `DB.cp` (DB.empty :: DB.Table Char) `shouldBe` (DB.empty :: DB.Table (Bool, Char))
  describe "Database.Bag neutral" $ do -- TODO:: add more tests when understood
    it "returns a bag with the unit element" $ do
      DB.neutral `shouldBe` Bag.Bag [()]
  describe "Database.Bag projection" $ do
    it "can select a certain column from a record" $ do
      DB.projection lastName people `shouldBe` Bag.Bag ["Doe", "Doe", "Smith"]
