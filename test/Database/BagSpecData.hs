module Database.BagSpecData where

import Data.Array
import Data.Key as Map (Map (A, Lone), empty)
import Data.Word
import Database.Bag

type Name = String

data Person = Person {firstName :: Name, lastName :: Name} deriving (Show, Eq)

people = fromList [Person "John" "Doe", Person "Jane" "Doe", Person "John" "Smith"]

lastNameJoin =
    fromList
        [ (Person "John" "Doe", Person "John" "Doe")
        , (Person "John" "Doe", Person "Jane" "Doe")
        , (Person "Jane" "Doe", Person "Jane" "Doe")
        , (Person "Jane" "Doe", Person "John" "Doe")
        , (Person "John" "Smith", Person "John" "Smith")
        ]

data NameNum = NN {name :: Name, nums :: Word16} deriving (Show, Eq)

namenums = fromList [NN "John" 12, NN "Jane" 12, NN "John" 18]

namenumjoins =
    fromList
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
    fromList
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems1 =
    fromList
        [ OrderItem "Apple" 1 23
        , OrderItem "Banana" 2 12
        ]

orderJoin1 =
    fromList -- orderPrices join OrderItems by id
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 23)
        , (OrderInvoice 2 15.20, OrderItem "Banana" 2 12)
        ]

-- Tests join when there are no matching elements per id
orderPrices2 =
    fromList
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems2 =
    fromList
        [ OrderItem "Apple" 4 23
        , OrderItem "Banana" 5 12
        ]

-- Tests join when there are multiple records in a single table with the same Id
orderPrices3 =
    fromList
        [ OrderInvoice 1 25.50
        , OrderInvoice 2 15.20
        , OrderInvoice 3 12.12
        ]

orderItems3 =
    fromList
        [ OrderItem "Apple" 1 11
        , OrderItem "Orange" 1 5
        , OrderItem "Peach" 1 12
        , OrderItem "Milk" 2 2
        , OrderItem "Banana" 2 12
        , OrderItem "Chocolate" 3 5
        ]

orderItems3kvps =
    [ (1, fromList [OrderItem "Apple" 1 11, OrderItem "Orange" 1 5, OrderItem "Peach" 1 12])
    , (2, fromList [OrderItem "Milk" 2 2, OrderItem "Banana" 2 12])
    , (3, fromList [OrderItem "Chocolate" 3 5])
    ]

orderItems3Array = A (accumArray (curry union) (Database.Bag.empty :: Table OrderItem) (0, 2 ^ 16 - 1) orderItems3kvps)

orderJoin3 =
    fromList -- orderPrices join OrderItems by id
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 11)
        , (OrderInvoice 1 25.50, OrderItem "Orange" 1 5)
        , (OrderInvoice 1 25.50, OrderItem "Peach" 1 12)
        , (OrderInvoice 2 15.20, OrderItem "Banana" 2 12)
        , (OrderInvoice 2 15.20, OrderItem "Milk" 2 2)
        , (OrderInvoice 3 12.12, OrderItem "Chocolate" 3 5)
        ]

-- for ease of testing breaking index as orderId
orderPrices4 =
    fromList
        [ OrderInvoice 1 25.50
        , OrderInvoice 1 15.20
        ]

orderItems4 =
    fromList
        [ OrderItem "Apple" 1 23
        , OrderItem "Banana" 1 12
        ]

orderJoin4 =
    fromList
        [ (OrderInvoice 1 25.50, OrderItem "Apple" 1 23)
        , (OrderInvoice 1 25.50, OrderItem "Banana" 1 12)
        , (OrderInvoice 1 15.20, OrderItem "Apple" 1 23)
        , (OrderInvoice 1 15.20, OrderItem "Banana" 1 12)
        ]
