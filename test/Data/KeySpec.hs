module Data.KeySpec (spec) where

import Data.Array
import qualified Data.Bag as Bag
import Data.Key
import qualified Data.PointedSet as Pointed
import Data.Word
import Test.Hspec

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
        it "can correctly identify the codomain of an empty map" $ do
            cod (Lone (Nothing :: Maybe Bool)) `shouldBe` Bag.empty
        it "can lookup elements" $ do
            Data.Key.lookup (Lone (Just True)) () `shouldBe` Just True
        it "can index a bag of pairs without multiplicity of keys" $ do
            Data.Key.index (Bag.Bag [((), Just 'r')]) `shouldBe` Lone (Bag.Bag [Just 'r'])
        it "can index an empty bag, with PointedSet null value as the null bag" $ do
            Data.Key.index (Bag.empty :: Bag.Bag ((), Int)) `shouldBe` Lone (Pointed.null :: Bag.Bag Int)
        it "can index a bag of pairs with multiplicity" $ do
            Data.Key.index (Bag.Bag [((), 'a'), ((), 'b'), ((), 'c')]) `shouldBe` Lone (Bag.Bag ['a', 'b', 'c'])
        it "can unindex a map with no multiplicities" $ do
            unindex (Lone (Bag.Bag [1])) `shouldBe` Bag.Bag [((), 1)]
        it "can unindex an empty map" $ do
            unindex (Lone (Pointed.null :: Bag.Bag Char)) `shouldBe` (Bag.empty :: Bag.Bag ((), Char))
        it "can unindex a map with multiplicities" $ do
            unindex (Lone (Bag.Bag [True, True, False])) `shouldBe` Bag.Bag [((), True), ((), True), ((), False)]
        it "can reduce a map correctly" $ do
            reduce (Lone (Bag.Bag ['a'])) `shouldBe` Bag.Bag ['a']
    describe "Map Word16" $ do
        it "correctly produces an empty map" $ do
            (empty :: Map Word16 (Maybe Int)) `shouldBe` A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [])
        it "can correctly detect an empty map" $ do
            isEmpty (A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) []) :: Map Word16 (Maybe Int)) `shouldBe` True
        it "detects its created empty map as empty" $ do
            isEmpty (empty :: Map Word16 (Maybe Int)) `shouldBe` True
        it "correctly produces a singleton with a non null value" $ do
            single (5, Just 'c') `shouldBe` A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, Just 'c')])
        it "correctly produces a singleton with a null value" $ do
            single (5, (Nothing :: Maybe Int)) `shouldBe` A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, (Nothing :: Maybe Int))])
        it "correctly merges two singletons with different indices" $ do
            merge ((single (5, Just 'd')), (single (6, Just 'e'))) `shouldBe` A (accumArray (curry snd) (Nothing, Nothing) (0, 2 ^ 16 - 1) [(5, (Just 'd', Nothing)), (6, (Nothing, Just 'e'))])
        it "correctly merges two singletons with the same index" $ do
            merge ((single (5, Just "one")), (single (5, Just "two"))) `shouldBe` A (accumArray (curry snd) (Nothing, Nothing) (0, 2 ^ 16 - 1) [(5, (Just "one", Just "two"))])
        it "correctly merges with one null value in the key" $ do
            merge ((single (5, Just "one")), (single (5, (Nothing :: Maybe String)))) `shouldBe` A (accumArray (curry snd) (Nothing, Nothing) (0, 2 ^ 16 - 1) [(5, (Just "one", (Nothing :: Maybe String)))])
        it "merging with two empty values produces an empty map" $ do
            merge ((single (5, (Nothing :: Maybe String))), (single (5, (Nothing :: Maybe String)))) `shouldBe` (empty :: Map Word16 (Maybe String, Maybe String))
        it "can correctly unmerge a map" $ do
            unmerge (A (accumArray (curry snd) (Nothing, Nothing) (0, 2 ^ 16 - 1) [(5, (Just "one", Just "two")), (6, (Just "three", Just "four"))])) `shouldBe` (A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, Just "one"), (6, Just "three")]), A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, Just "two"), (6, Just "four")]))
        it "can correctly unmerge an empty map" $ do
            unmerge (empty :: Map Word16 (Maybe String, Maybe Bool)) `shouldBe` (empty :: Map Word16 (Maybe String), empty :: Map Word16 (Maybe Bool))
        it "can correctly find the domain of a map" $ do
            dom (A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, Just 'd'), (6, Just 'e')])) `shouldBe` Bag.Bag [5, 6]
        it "can correctly find the domain of an empty map" $ do
            dom (empty :: Map Word16 (Maybe String)) `shouldBe` (Bag.empty :: Bag.Bag Word16)
        it "can correctly find the domain of a map with only null values" $ do
            dom (single (5, Nothing)) `shouldBe` (Bag.empty :: Bag.Bag Word16)
        it "can correctly find the codomain of a map" $ do
            cod (A (accumArray (curry snd) Nothing (0, 2 ^ 16 - 1) [(5, Just 'd'), (6, Just 'e')])) `shouldBe` Bag.Bag [Just 'd', Just 'e']
        it "can correctly find the codomain of an empty map" $ do
            cod (empty :: Map Word16 (Maybe Bool)) `shouldBe` (Bag.empty :: Bag.Bag (Maybe Bool))
        it "can correctly find the codomain of a map with only null elements" $ do
            cod (single ((5 :: Word16), (Nothing :: Maybe Int))) `shouldBe` (Bag.empty :: Bag.Bag (Maybe Int))
        it "can lookup a singleton with a present key" $ do
            Data.Key.lookup (single (5 :: Word16, Just 3)) 5 `shouldBe` Just 3
        it "can lookup a singleton with a key not present" $ do
            Data.Key.lookup (single (5 :: Word16, Just 3)) 4 `shouldBe` (Pointed.null :: Maybe Int)
        it "can lookup an empty map" $ do
            Data.Key.lookup (empty :: Map Word16 (Bag.Bag Int)) 2 `shouldBe` (Pointed.null :: Bag.Bag Int)
        it "can create a map from a bag of key value pairs with distinct indices" $ do
            Data.Key.index (Bag.Bag [(1, Just '1'), (2, Just '2')]) `shouldBe` A (accumArray (curry snd) (Bag.empty :: Bag.Bag (Maybe Char)) (0, 2 ^ 16 - 1) [(1, Bag.single (Just '1')), (2, Bag.single (Just '2'))])
        it "can create a map from a bag of key value pairs with shared indices" $ do
            Data.Key.index (Bag.Bag [(1, Just '1'), (2, Just '2'), (1, Just '3'), (2, Just '4')]) `shouldBe` A (accumArray (curry snd) (Bag.empty :: Bag.Bag (Maybe Char)) (0, 2 ^ 16 - 1) [(1, Bag.Bag [Just '1', Just '3']), (2, Bag.Bag [Just '2', Just '4'])])
        it "can index an empty bag" $ do
            Data.Key.index (Bag.empty :: Bag.Bag (Word16, Maybe Char)) `shouldBe` (empty :: Map Word16 (Bag.Bag (Maybe Char)))
        --
        it "can unindex a map with singleton bag values" $ do
            Data.Key.unindex (A (accumArray (curry snd) (Bag.empty :: Bag.Bag (Maybe Char)) (0, 2 ^ 16 - 1) [(1, Bag.single (Just '1')), (2, Bag.single (Just '2'))])) `shouldBe` Bag.Bag [(1, Just '1'), (2, Just '2')]
        it "can unindex a map with bags with multiple values" $ do
            Data.Key.unindex (A (accumArray (curry snd) (Bag.empty :: Bag.Bag (Maybe Char)) (0, 2 ^ 16 - 1) [(1, Bag.Bag [Just '1', Just '1']), (2, Bag.Bag [Just '2', Just '4'])])) `shouldBe` (Bag.Bag [(1, Just '1'), (2, Just '2'), (1, Just '1'), (2, Just '4')])
        it "can index unindex an empty map" $ do
            Data.Key.unindex (empty :: Map Word16 (Bag.Bag (Maybe Char))) `shouldBe` (Bag.empty :: Bag.Bag (Word16, Maybe Char))
        it "can reduce a map with singleton bags" $ do
            Data.Key.reduce (A (accumArray (curry snd) (Bag.empty :: Bag.Bag Bool) (0, 2 ^ 16 - 1) [(1, Bag.single (True)), (2, Bag.single (False))])) `shouldBe` Bag.Bag [True, False]
        it "can reduce a map with multiple value bags bags" $ do
            Data.Key.reduce (A (accumArray (curry snd) (Bag.empty :: Bag.Bag Int) (0, 2 ^ 16 - 1) [(1, Bag.Bag [1, 2]), (2, Bag.Bag [2, 5, 6])])) `shouldBe` Bag.Bag [1, 2, 2, 5, 6]
        it "can reduce an empty map" $ do
            (Data.Key.empty :: Map Word16 (Bag.Bag Int)) `shouldBe` (empty :: Map Word16 (Bag.Bag Int))
