module Database.BagSpec (spec) where

import qualified Data.Bag as Bag (Bag, empty)
import Data.Key as Map (Map (A, Lone), empty)
import Data.Monoid
import Data.Word
import Database.Bag
import qualified Database.Bag as DB
import Database.BagSpecData
import Test.Hspec

spec :: Spec
spec = do
    describe "Database.Bag empty" $
        it "produces an empty Bag of the same type" $
            (DB.empty :: Table Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
    describe "Database.Bag single" $
        it "produces a Bag with a single element" $
            single 5 `shouldBe` fromList [5]
    describe "Database.Bag fromList" $
        it "produces a Bag from a list" $
            fromList [1, 2] `shouldBe` union (single 1, single 2)
    describe "Database.Bag union" $ do
        it "uses the bag union to calculate the union" $
            union (fromList [1, 2, 3], fromList [3, 4, 5]) `shouldBe` fromList [1, 2, 3, 3, 4, 5]
        it "can correctly deal with one empty table in the union" $
            union (fromList [1, 2, 3], DB.empty) `shouldBe` fromList [1, 2, 3]
        it "can find the union of two DB.empty tables" $
            union (DB.empty :: Table Char, DB.empty) `shouldBe` (DB.empty :: Table Char)
    describe "Database.Bag cp" $ do
        it "correctly can calculate the cartesian product of two tables" $
            cp (fromList [1, 2], fromList [3, 4]) `shouldBe` fromList [(1, 3), (1, 4), (2, 3), (2, 4)]
        it "returns an empty table when one table is empty" $
            cp (fromList [1, 2], DB.empty :: Table Char) `shouldBe` (DB.empty :: Table (Int, Char))
        it "returns an empty table when both tables are empty" $
            cp (DB.empty :: Table Bool, DB.empty :: Table Char) `shouldBe` (DB.empty :: Table (Bool, Char))
    describe "Database.Bag neutral" $ do
        -- TODO:: add more tests when understood
        it "returns a bag with the unit element" $
            neutral `shouldBe` fromList [()]
    describe "Database.Bag project" $
        it "can select a certain column from a record" $
            project lastName people `shouldBe` fromList ["Doe", "Doe", "Smith"]
    describe "Database.Bag select" $ do
        it "can select a subset of the table" $
            select (\p -> lastName p == "Doe") people `shouldBe` fromList [Person "John" "Doe", Person "Jane" "Doe"]
        it "can empty a whole table if necessary" $
            select (const False) people `shouldBe` fromList []
        it "can select the whole table" $
            select (const True) people `shouldBe` people
    describe "Database.Bag aggregate" $ do
        it "can correctly aggregate a table without multiplicities" $
            (getAny . aggregate) (fromList [Any True, Any False]) `shouldBe` True
        it "can correctly aggregate a table with multiplicities" $
            aggregate (fromList [1, 1, 1, 1, 2] :: Table (Sum Int)) `shouldBe` 6
    describe "Database.Bag productEquijoin" $ equijoinTest productEquijoin
    describe "indexBy" $ do
        it "can correctly index with trivial key" $
            people `indexBy` const () `shouldBe` Map.Lone people
        it "can correctly index an empty bag" $
            (DB.empty :: Table Int) `indexBy` const () `shouldBe` (Map.empty :: Map () (Table Int))
        it "can correctly index a bag with a repeated index" $
            orderItems3 `indexBy` (fromIntegral . orderId :: OrderItem -> Word16) `shouldBe` orderItems3Array
    describe "Database.Bag indexedEquijoin" $ equijoinTest indexedEquijoin
    describe "Database.Bag comprehensionEquijoin" $ equijoinTest comprehensionEquijoin

equijoinTest equijoin = do
    it "can join two tables with at most one matching element" $
        equijoin invoiceId orderId (orderPrices1, orderItems1) `shouldBe` orderJoin1
    it "can join two tables with more than one matching elements, only multiple in one table" $
        equijoin invoiceId orderId (orderPrices3, orderItems3) `shouldBe` orderJoin3
    it "can join two tables with no matching elements" $
        equijoin invoiceId orderId (orderPrices2, orderItems2) `shouldBe` Bag.empty
    it "can join two tables with multiple elements in both tables" $
        equijoin invoiceId orderId (orderPrices4, orderItems4) `shouldBe` orderJoin4
