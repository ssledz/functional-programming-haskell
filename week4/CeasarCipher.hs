module CeaserCipher (cipher, decipher, betterCipher, betterDecipher) where

import Data.Char

shouldCipher :: Char -> Bool
shouldCipher c = isLetter c && isAscii c


cipherChar :: Int -> Char -> Char
cipherChar shift c
  | shouldCipher c = chr(ord(c) + shift)
  | otherwise      = c

cipher :: Int -> String -> String
cipher shift plaintext = map (cipherChar shift) plaintext

decipher :: Int -> String -> String
decipher shift ciphertext = cipher (-shift) ciphertext

wraparound :: Int -> Char -> Bool
wraparound shift c
  | isLower c && ord c + shift  > ord 'z' = True
  | isUpper c && ord c + shift  > ord 'Z' = True
  | otherwise                             = False

betterCipherChar :: Int -> Char -> Char
betterCipherChar shift c
  | shouldCipher c = chr(ord(c) + adjustedShift)
  | otherwise      = c
  where adjustedShift = let shift' = shift `mod` 26
                        in if (wraparound shift' c)
                           then shift' - 26
                           else shift'

betterCipher :: Int -> String -> String
betterCipher shift plaintext = map (betterCipherChar shift) plaintext

betterDecipher :: Int -> String -> String
betterDecipher shift ciphertext = betterCipher (-shift) ciphertext


