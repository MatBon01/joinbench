module Text.Parser.Customers where

import Text.ParserCombinators.Parsec

type Identifier = Int
type Name = String
data Customer = C { cid :: Identifier, name :: Name }
