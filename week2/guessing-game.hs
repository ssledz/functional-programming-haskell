module Main where

import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
      [word] -> turn word (['_' | x <- word]) 3
      _ -> putStrLn "Please enter a word"

check :: String -> String -> Char -> (Bool, String)
check word disp c = (c `elem` word, [if x == c then x else y | (x, y) <- zip word disp])

turn :: String -> String -> Int -> IO ()
turn word disp n =
    do
      if n == 0 then
        putStrLn "You lose"
      else if word == disp then
        putStrLn "You win!"
      else
        do
          putStrLn ("Word: " ++ disp)
          mkguess word disp n

mkguess :: String -> String -> Int -> IO ()
mkguess word disp n =
  do
    putStr "Enter your guess: "
    hFlush stdout
    g <- getLine
    let (guessed, newDisp) = check word disp $ head g
    turn word newDisp $ if guessed then n else n - 1



