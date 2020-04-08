module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn (speller args)

speller :: [[Char]] -> [Char]

speller [] = ""
speller [word] = spell word
speller words =
  let x = last words
      xs = init words
      ys = join ", " (map spell xs)
  in ys ++ ", and " ++ (spell x)

spell :: [Char] -> [Char]
spell word = (head word):(" is for " ++ word)

join :: [Char] -> [[Char]] -> [Char]
join delim [] = ""
join delim (x:[]) = x
join delim (x:xs) = x ++ delim ++ join delim xs
