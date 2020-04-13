## Example of QuickCheck session in ghci

```
cabal install QuickCheck
cabat repl --build-depends "QuickCheck >= 2.14"
```

```
:load CeasarCipher.hs
quickCheck ((\n -> (\s -> (decipher n (cipher n s) == s))) :: Int -> String -> Bool)
*** Failed! Falsified (after 15 tests and 7 shrinks):    
7
"t"
quickCheck ((\n -> (\s -> (betterDecipher n (betterCipher n s) == s))) :: Int -> String -> Bool)
+++ OK, passed 100 tests.

verboseCheck ((\n -> (\s -> (betterDecipher n (betterCipher n s) == s))) :: Int -> String -> Bool)

Passed:
0
""

Passed:
1
"\1020791"

Passed:
2
""

Passed:
3
"u"

Passed:
-1
"\ACKy|"
```
