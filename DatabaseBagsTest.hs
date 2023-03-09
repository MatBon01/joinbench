type Name = String

type Age = Int

type BirthCountry = String

data TestDatabaseRecord = TestDatabaseRecord {name :: Name, age :: Age, birthCountry :: BirthCountry}
  deriving (Show)