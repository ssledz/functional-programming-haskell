module Main (main) where

import ShowParser (parseShow)

main = print $ parseShow (show [p1, p2])

a = Address "School of Haskell" 13 "Aspekt" "Warsaw" "01-101"
p1 = Person "John" a 1 [Green, Red]
p2 = Person "Anna" a 2 [Yellow]

data Person = Person {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]
} deriving (Show)

data Address = Address {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)




