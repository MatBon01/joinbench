module Data.BagSpec (spec) where

import Control.Applicative
import qualified Data.Bag as Bag
import Data.Monoid
import qualified Data.PointedSet as Pointed
import Test.Hspec

{- HLINT ignore "Monoid law, right identity" -}

-- Some test cases from https://www.cis.upenn.edu/~cis1940/spring13/lectures/
(.*) = liftA2 (*)

(.+) = liftA2 (+)

addOneOrTwo x = Bag.Bag [x + 1, x + 2]

-- Some examples for the test
b1 = Bag.Bag ['a', 'b', 'c']

b2 = Bag.Bag ['b', 'c', 'd']

b3 = Bag.Bag ['c', 'd', 'e']

spec :: Spec
spec = do
    describe "Data.Bag Eq" $ do
        it "singleton bags are equal" $ do
            Bag.Bag [1] `shouldBe` Bag.Bag [1]
        it "different singleton bags are not equal" $ do
            Bag.Bag ['a'] `shouldNotBe` Bag.Bag ['c']
        it "ordering does not matter" $ do
            Bag.Bag [1, 2, 3, 4, 5] `shouldBe` Bag.Bag [3, 2, 5, 1, 4]

        it "length and repetitions matter" $ do
            Bag.Bag [1, 2, 3, 3] `shouldNotBe` Bag.Bag [1, 2, 3]

        it "repetitions of specific elements matter" $ do
            Bag.Bag [1, 1, 2] `shouldNotBe` Bag.Bag [1, 2, 2]

        it "repetitions matter" $ do
            Bag.Bag [1, 2, 2, 3, 3, 3] `shouldBe` Bag.Bag [1, 2, 3, 2, 3, 3]

    describe "Data.Bag fmap" $ do
        it "maps over the underlying list" $ do
            fmap (5 +) (Bag.Bag [0, 10, 20, 30]) `shouldBe` Bag.Bag [5, 15, 25, 35]

    describe "Data.Bag pure" $ do
        it "raises the value to a Bag whose elements is a singleton list of the value" $ do
            (pure [23, 34] :: Bag.Bag [Int]) `shouldBe` Bag.Bag [[23, 34]]

    describe "Data.Bag <*>" $ do
        it "should as implemented in lists" $ do
            (Bag.Bag [4, 5] .* pure 2) .+ Bag.Bag [6, 1] `shouldBe` Bag.Bag [14, 9, 16, 11]

    describe "Data.Bag return" $ do
        it "should return a bag with a singleton list" $ do
            (return 72 :: Bag.Bag Int) `shouldBe` Bag.Bag [72]

    describe "Data.Bag >>=" $ do
        it "should be the same as in lists" $ do
            (Bag.Bag [11, 15] >>= addOneOrTwo) `shouldBe` Bag.Bag [12, 13, 16, 17]

    describe "Data.Bag.empty" $ do
        it "returns a bag with an empty list" $ do
            (Bag.empty :: Bag.Bag Int) `shouldBe` Bag.Bag []

    describe "Data.Bag.union" $ do
        it "returns the union of both bags" $ do
            Bag.union (Bag.Bag ['a', 'c', 'a'], Bag.Bag ['c', 'd']) `shouldBe` Bag.Bag ['a', 'c', 'a', 'c', 'd']

    describe "Data.Bag Semigroup" $ do
        it "has an associative operator <>" $ do
            (b1 <> b2) <> b3 `shouldBe` b1 <> (b2 <> b3)
    describe "Data.Bag Monoid" $ do
        it "has an identity, mempty" $ do
            (b1 <> mempty :: Bag.Bag Char) `shouldBe` b1
    describe "Data.Bag CMonoid" $ do
        it "has a commutative operator <>" $ do
            (b1 <> b2) `shouldBe` (b2 <> b1)
    describe "Data.Bag Pointed Set" $ do
        it "has empty as its null" $ do
            (Pointed.null :: Bag.Bag Int) `shouldBe` (Bag.empty :: Bag.Bag Int)
        it "has mempty as its null" $ do
            (Pointed.null :: Bag.Bag Char) `shouldBe` (mempty :: Bag.Bag Char)
        it "correctly identifies null" $ do
            Pointed.isNull (Bag.empty :: Bag.Bag Int) `shouldBe` True
        it "does not identify a non-empty Bag as null" $ do
            Pointed.isNull (Bag.Bag [1, 2, 4]) `shouldBe` False
    describe "Data.Bag.reduceBag" $ do
        it "correctly reduces a bag using sum" $ do
            (getSum . Bag.reduceBag) (Bag.Bag [1, 2, 3, 4] :: Bag.Bag (Sum Int)) `shouldBe` 10
        it "correctly reduces a bag using product" $ do
            (getProduct . Bag.reduceBag) (Bag.Bag [1, 2, 3, 4] :: Bag.Bag (Product Int)) `shouldBe` 24
    describe "Data.Bag.cp" $ do
        it "correctly can calculate the cartesian product of two bags" $ do
            Bag.cp (b1, b2) `shouldBe` Bag.Bag [('a', 'b'), ('a', 'c'), ('a', 'd'), ('b', 'b'), ('b', 'c'), ('b', 'd'), ('c', 'b'), ('c', 'c'), ('c', 'd')]
        it "correctly deals with the empty bag in a cartesian product" $ do
            Bag.cp (b1, Bag.empty :: Bag.Bag Int) `shouldBe` (Bag.empty :: Bag.Bag (Char, Int))
    describe "Data.Bag.single" $ do
        it "creates a singleton bag" $ do
            Bag.single 'a' `shouldBe` Bag.Bag ['a']
    describe "Data.Bag.filter" $ do
        it "can filter a bag without multiplicities" $ do
            Bag.filter (== 'a') b1 `shouldBe` Bag.Bag ['a']
        it "can filter a bag and maintain multiplicities" $ do
            Bag.filter even (Bag.Bag [1, 2, 4, 2, 3, 4, 4, 6]) `shouldBe` Bag.Bag [2, 2, 4, 4, 4, 6]
