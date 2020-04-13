"Guards, Guards!"

:set +m

let {
  absolute x
    | x < 0 = -x
    | otherwise = x
}

absolute (-1)
absolute 1


let {
  holeScore strokes par
    | score < 0 = show (abs score) ++ " under par"
    | score == 0 = "level par"
    | otherwise = show(score) ++ " over par"
   where score = strokes-par
}

holeScore 1 1


data Pet = Dog | Cat | Fish | Parrot String

let {
  hello x = case x of
    Dog -> "woof"
    Cat -> "meeow"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name
}

hello Dog
hello (Parrot "polly")

:load max.hs

max' []
max' [1,2,5,1,2]

let inc = (+1)

fmap inc (Just 1)
fmap inc $ Just 2
fmap inc Nothing

"Practical parsing"

let {
  sayHello name = do
    let str = "Hello " ++ name
    print str
    return $ "ret: " ++ str
}

sayHello "John"

import Text.ParserCombinators.Parsec

data Tag = MkTag String deriving Show

identifier = many (noneOf ['<', '>']) :: Parser String
parseTag = do { char '<'; x <- identifier; char '>'; return (MkTag x) } :: Parser Tag

parseTest parseTag "<div>"

parseTest (char 'c') "const"


parseDiv = do { string "<div>"; return (MkTag "div") } :: Parser Tag

parseTest parseDiv "<div>Hello World</div>"

letterOrDigit = do { x <- letter <|> digit; return x} :: Parser Char

parseTest letterOrDigit "1"
parseTest letterOrDigit "a"

bagOrBog = try (string "bag") <|> string "bog" :: Parser String
parseTest bagOrBog "bag"
parseTest bagOrBog "bog"


varname = do { x <- letter; xs <- many (letter <|> digit); return (x:xs) } :: Parser String
parseTest varname "ab123"
--parseTest varname "1b123"

Just 1 >> Just 2
Just 1 >>= \x -> Just (x + 1)



