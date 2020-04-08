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
      ys = foldr (\w acc -> ", " ++ w ++ acc) "" (map spell xs)
  in (tail (tail ys)) ++ ", and " ++ (spell x)

spell :: [Char] -> [Char]
spell word = (head word):(" is for " ++ word)
