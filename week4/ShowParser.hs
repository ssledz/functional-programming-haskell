module ShowParser ( parseShow) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List ( intercalate )

runShowParser :: Parser a -> String -> a
runShowParser p str = case parse p "" str of
  Left err -> error $ "parse error at " ++ (show err)
  Right val -> val

xmlHeader =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
parseShow :: String -> String
parseShow str = xmlHeader ++ (runShowParser showParser str)


otag t = "<" ++ t ++ ">"
ctag t = "</" ++ t ++ ">"
tag t v = concat [otag t, v, ctag t]
tagAttrs :: String -> [(String, String)] -> String -> String
tagAttrs t attrs v = concat [otag (unwords $ [t] ++ map (\(k, v) -> concat [k,"=", "\"", v, "\""]) attrs), v, ctag t]

join d arr = intercalate d arr
joinNL = join "\n"

showParser :: Parser String
showParser =
  listParser <|>
  tupleParsr <|>
  try recordParser <|>
  adtParser <|>
  numberParser <|>
  quatedStringParser <?> "Parse error"

adtParser = do
  ti <- typeIdentifierParser
  return $ tag "atd" ti

listParser = do
  ls <- brackets $ commaSep showParser
  return $ tag "list" $ joinNL $ map (tag "list-elt") ls

tupleParsr = do
  ls <- parens $ commaSep showParser
  return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

recordParser = do
  ti <- typeIdentifierParser
  ls <- braces $ commaSep kvParser
  return $ tagAttrs "record" [("name", ti)] (joinNL ls)

kvParser = do
  k <- identifier
  symbol "="
  t <- showParser
  return $ tagAttrs "elt" [("key", k)] t

typeIdentifierParser = do
  fst <- oneOf ['A' .. 'Z']
  rest <- many alphaNum
  whiteSpace
  return $ fst:rest

numberParser = do
  n <- integer
  return $ show n

quatedStringParser = do
  s <- stringLiteral
  return $ "\"" ++ s ++ "\""

lexer           = P.makeTokenParser emptyDef
parens          = P.parens lexer
brackets        = P.brackets lexer
braces          = P.braces lexer
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
integer         = P.integer lexer
stringLiteral   = P.stringLiteral lexer
