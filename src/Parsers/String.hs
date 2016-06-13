module Parsers.String (string) where

import Data.Functor (($>))

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import Data.Lisp (LispVal(LispString))

escapedChar :: Parsec.Parser Char
escapedChar =
  Parsec.char '\\' >> (
    Parsec.oneOf "\"\"" <|>
    (Parsec.char 'n' $> '\n') <|>
    (Parsec.char 'r' $> '\r') <|>
    (Parsec.char 't' $> '\t')
  )

string :: Parsec.Parser LispVal
string = do
  Parsec.char '"'
  x <- Parsec.many ((Parsec.noneOf "\"") <|> escapedChar)
  Parsec.char '"'
  return $ LispString x
