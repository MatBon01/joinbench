module Main where

import qualified Data.MultiSet as MultiSet

type Name = String

type Age = Int

type Table = MultiSet.MultiSet

type BirthCountry = String

data Record = Record {name :: Name, age :: Age, birthCountry :: BirthCountry}
  deriving (Show)

main :: IO ()
main = do
  putStrLn "This is a database bags test"
  putStrLn (show (MultiSet.insertMany 12 3 MultiSet.empty))

newTable :: Table Record
newTable = MultiSet.empty

singletonTable :: Record -> Table Record
singletonTable = MultiSet.singleton