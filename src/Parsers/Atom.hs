module Parsers.Atom (atom) where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import Data.Lisp (LispVal(LispAtom))

symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!$%&|*+-/:<=?>@^_~#"


atom :: Parsec.Parser LispVal
atom = do
  first <- Parsec.letter <|> symbol
  rest <- Parsec.many (Parsec.letter <|> Parsec.digit <|> symbol)
  let atom = [first] ++ rest
  return $ LispAtom atom
