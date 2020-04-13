import Test.QuickCheck
import Data.List

quickCheck ((\n -> abs(n) == n) :: Int -> Bool)

quickCheck ((\n -> (abs(n) == n) || (abs(n)) == -n ) :: Int -> Bool)

quickCheck ((\xs -> head (sort xs) == minimum xs) :: [Int] -> Bool)

quickCheck ((\xs -> if xs == [] then True else (head (sort xs) == minimum xs)) :: [Int] -> Bool)

quickCheck ((\xs -> if xs == [] then True else (head (sort xs) == minimum xs)) :: [Char] -> Bool)
