module Main where

import qualified Data.MultiSet as MultiSet

type Name = String

type Age = Int

type BirthCountry = String

data TestDatabaseRecord = TestDatabaseRecord {name :: Name, age :: Age, birthCountry :: BirthCountry}
  deriving (Show)

main :: IO ()
main = do
  putStrLn "This is a database bags test"
  putStrLn (show (MultiSet.insertMany 12 3 MultiSet.empty))
