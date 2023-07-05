module Database.BagSpec (spec) where

import Data.Array
import qualified Data.Bag as Bag
import Data.Key as Map
import Data.Monoid
import Data.Word
import Database.Bag (comprehensionEquijoin, indexedEquijoin, productEquijoin)
import qualified Database.Bag as DB
import Test.Hspec

type Name = String

data Person = Person {firstName :: Name, lastName :: Name} deriving (Show, Eq)

people = Bag.Bag [Person "John" "Doe", Person "Jane" "Doe", Person "John" "Smith"]

lastNameJoin =
    Bag.Bag
        [ (Person "John" "Doe", Person "John" "Doe")
        , (Person "John" "Doe", Person "Jane" "Doe")
        , (Person "Jane" "Doe", Person "Jane" "Doe")
        , (Person "Jane" "Doe", Person "John" "Doe")
        , (Person "John" "Smith", Person "John" "Smith")
        ]

data NameNum = NN {name :: Name, nums :: Word16} deriving (Show, Eq)

namenums = Bag.Bag [NN "John" 12, NN "Jane" 12, NN "John" 18]

namenumjoins =
    Bag.Bag
        [ (NN "John" 12, NN "John" 12)
        , (NN "John" 12, NN "Jane" 12)
        , (NN "Jane" 12, NN "Jane" 12)
        , (NN "Jane" 12, NN "John" 12)
        , (NN "John" 18, NN "John" 18)
        ]

type OrderId = Word16

type OrderPrice = Float

type Item = String

type ItemQuantity = Word16

data OrderInvoice = OrderInvoice {invoiceId :: OrderId, orderPrice :: OrderPrice} deriving (Show, Eq)

-- Purposefully different id name from invoice for testing variation
data OrderItem = OrderItem {item :: Item, orderId :: OrderId, quantity :: ItemQuantity} deriving (Show, Eq)

-- Tests join when there is at most one matching element per id
orderPrices1 =
    Bag.Bag
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems1 =
    Bag.Bag
        [ OrderItem "Apple" 1 23
        , OrderItem "Banana" 2 12
        ]

orderJoin1 =
    Bag.Bag -- orderPrices join OrderItems by id
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 23)
        , (OrderInvoice 2 15.20, OrderItem "Banana" 2 12)
        ]

-- Tests join when there are no matching elements per id
orderPrices2 =
    Bag.Bag
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems2 =
    Bag.Bag
        [ OrderItem "Apple" 4 23
        , OrderItem "Banana" 5 12
        ]

-- Tests join when there are multiple records in a single table with the same Id
orderPrices3 =
    Bag.Bag
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems3 =
    Bag.Bag
        [ OrderItem "Apple" 1 11
        , OrderItem "Orange" 1 5
        , OrderItem "Peach" 1 12
        , OrderItem "Milk" 2 2
        , OrderItem "Banana" 2 12
        , OrderItem "Chocolate" 3 5
        ]

orderItems3kvps =
    [ (1, Bag.Bag [OrderItem "Apple" 1 11, OrderItem "Orange" 1 5, OrderItem "Peach" 1 12])
    , (2, Bag.Bag [OrderItem "Milk" 2 2, OrderItem "Banana" 2 12])
    , (3, Bag.Bag [OrderItem "Chocolate" 3 5])
    ]

orderItems3Array = A (accumArray (curry Bag.union) (Bag.empty :: Bag.Bag OrderItem) (0, 2 ^ 16 - 1) orderItems3kvps)

orderJoin3 =
    Bag.Bag -- orderPrices join OrderItems by id
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 11)
        , (OrderInvoice 1 25.50, OrderItem "Orange" 1 5)
        , (OrderInvoice 1 25.50, OrderItem "Peach" 1 12)
        , (OrderInvoice 2 15.20, OrderItem "Banana" 2 12)
        , (OrderInvoice 2 15.20, OrderItem "Milk" 2 2)
        , (OrderInvoice 3 12.12, OrderItem "Chocolate" 3 5)
        ]

-- for ease of testing breaking index as orderId
orderPrices4 =
    Bag.Bag
        [ OrderInvoice 1 25.50
        , OrderInvoice 1 15.20
        ]

orderItems4 =
    Bag.Bag
        [ OrderItem "Apple" 1 23
        , OrderItem "Banana" 1 12
        ]

orderJoin4 =
    Bag.Bag
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 23)
        , (OrderInvoice 1 25.50, OrderItem "Banana" 1 12)
        , (OrderInvoice 1 15.20, OrderItem "Apple" 1 23)
        , (OrderInvoice 1 15.20, OrderItem "Banana" 1 12)
        ]

spec :: Spec
spec = do
    describe "Database.Bag empty" $ it "produces an empty Bag of the same type" $ (DB.empty :: DB.Table Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
    describe "Database.Bag single" $ it "produces a Bag with a single element" $ DB.single 5 `shouldBe` Bag.Bag [5]
    describe "Database.Bag fromList" $
        it "produces a Bag from a list" $
            DB.fromList [1, 2] `shouldBe` DB.union (DB.single 1, DB.single 2)
    describe "Database.Bag union" $ do
        it "uses the bag union to calculate the union" $ DB.union (Bag.Bag [1, 2, 3], Bag.Bag [3, 4, 5]) `shouldBe` Bag.Bag [1, 2, 3, 3, 4, 5]
        it "can correctly deal with one empty table in the union" $ DB.union (Bag.Bag [1, 2, 3], DB.empty) `shouldBe` Bag.Bag [1, 2, 3]
        it "can find the union of two empty tables" $ DB.union (DB.empty :: DB.Table Char, DB.empty) `shouldBe` (DB.empty :: DB.Table Char)
    describe "Database.Bag cp" $ do
        it "correctly can calculate the cartesian product of two tables" $ DB.cp (Bag.Bag [1, 2], Bag.Bag [3, 4]) `shouldBe` Bag.Bag [(1, 3), (1, 4), (2, 3), (2, 4)]
        it "returns an empty table when one table is empty" $ DB.cp (Bag.Bag [1, 2], DB.empty :: DB.Table Char) `shouldBe` (DB.empty :: DB.Table (Int, Char))
        it "returns an empty table when both tables are empty" $ DB.cp (DB.empty :: DB.Table Bool, DB.empty :: DB.Table Char) `shouldBe` (DB.empty :: DB.Table (Bool, Char))
    describe "Database.Bag neutral" $ do
        -- TODO:: add more tests when understood
        it "returns a bag with the unit element" $ DB.neutral `shouldBe` Bag.Bag [()]
    describe "Database.Bag project" $ it "can select a certain column from a record" $ DB.project lastName people `shouldBe` Bag.Bag ["Doe", "Doe", "Smith"]
    describe "Database.Bag select" $ do
        it "can select a subset of the table" $ DB.select (\p -> lastName p == "Doe") people `shouldBe` Bag.Bag [Person "John" "Doe", Person "Jane" "Doe"]
        it "can empty a whole table if necessary" $ DB.select (const False) people `shouldBe` Bag.Bag []
        it "can select the whole table" $ DB.select (const True) people `shouldBe` people
    describe "Database.Bag aggregate" $ do
        it "can correctly aggregate a table without multiplicities" $ (getAny . DB.aggregate) (Bag.Bag [Any True, Any False]) `shouldBe` True
        it "can correctly aggregate a table with multiplicities" $ DB.aggregate (Bag.Bag [1, 1, 1, 1, 2] :: Bag.Bag (Sum Int)) `shouldBe` 6
    describe "Database.Bag productEquijoin" $ equijoinTest productEquijoin
    describe "indexBy" $ do
        it "can correctly index with trivial key" $ people `DB.indexBy` const () `shouldBe` Map.Lone people
        it "can correctly index an empty bag" $ (DB.empty :: DB.Table Int) `DB.indexBy` const () `shouldBe` (Map.empty :: Map () (Bag.Bag Int))
        it "can correctly index a bag with a repeated index" $ orderItems3 `DB.indexBy` (fromIntegral . orderId :: OrderItem -> Word16) `shouldBe` orderItems3Array
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
