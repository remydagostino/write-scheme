module Parsers.Expression (expression) where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import Data.Lisp (LispVal(LispBool))
import qualified Parsers.Number as ParseNumber
import qualified Parsers.String as ParseString
import qualified Parsers.Character as ParseChar
import qualified Parsers.Atom as ParseAtom

-- Parses all the things that start with a "#"
parseHashSymbol :: Parsec.Parser LispVal
parseHashSymbol =
  Parsec.char '#' >>
  (
    -- Characters
    ((Parsec.char '\\') >> ParseChar.character) <|>
    -- Numbers in bases
    ParseNumber.numberWithBasePrefix <|>
    -- Booleans
    ((Parsec.char 't') >> return (LispBool True)) <|>
    ((Parsec.char 'f') >> return (LispBool False))
  )


expression :: Parsec.Parser LispVal
expression =
  ParseString.string <|>
  parseHashSymbol <|>
  ParseNumber.decimalNumber <|>
  ParseAtom.atom



