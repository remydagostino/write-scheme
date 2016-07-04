module Parsers.List (inParens, dottedList, expression) where

import Control.Monad (liftM)

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import Data.Lisp (LispVal(LispList, LispDottedList, LispAtom))
import qualified Parsers.Expression as ParseExp

spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space


-- A bigger version of Parsers.Expression that supports lists
expression :: Parsec.Parser LispVal
expression =
  quoted <|>
  inParens <|>
  ParseExp.expression


list :: Parsec.Parser LispVal
list =
  liftM LispList $ Parsec.sepBy expression spaces


dottedList :: Parsec.Parser LispVal
dottedList = do
  head <- Parsec.endBy expression spaces
  tail <- Parsec.char '.' >> spaces >> expression
  return $ LispDottedList head tail


quoted :: Parsec.Parser LispVal
quoted = do
  Parsec.char '\''
  x <- expression
  return $ LispList [LispAtom "quote", x]


inParens :: Parsec.Parser LispVal
inParens = do
  Parsec.char '('
  x <- Parsec.try list <|> dottedList
  Parsec.char ')'
  return x


