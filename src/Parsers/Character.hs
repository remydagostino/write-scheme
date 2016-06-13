module Parsers.Character where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import Data.Lisp (LispVal(LispChar))

character :: Parsec.Parser LispVal
character =
  ((Parsec.string "space")   >> return (LispChar ' ')) <|>
  ((Parsec.string "newline") >> return (LispChar '\n')) <|>
  ((Parsec.char '(')         >> return (LispChar '(')) <|>
  ((Parsec.char ' ')         >> return (LispChar ' ')) <|>
  (Parsec.letter            >>= (return . LispChar))


