module Data.BagSpec (spec) where

import Test.Hspec
import qualified Data.Bag as Bag

import Control.Applicative

-- Some test cases from https://www.cis.upenn.edu/~cis1940/spring13/lectures/
(.*) = liftA2 (*)
(.+) = liftA2 (+)
addOneOrTwo x = Bag.Bag [x + 1, x + 2]

spec :: Spec
spec = do
  describe "Data.Bag Eq" $ do
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
      (Bag.Bag [4,5] .* pure 2) .+ Bag.Bag [6,1] `shouldBe` Bag.Bag [14, 9, 16, 11]
  
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
      (Bag.Bag ['a', 'c', 'a'] `Bag.union` Bag.Bag ['c', 'd']) `shouldBe` Bag.Bag ['a', 'c', 'a', 'c', 'd']

