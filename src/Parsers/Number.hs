module Parsers.Number (numberWithBasePrefix, decimalNumber) where

import Data.Maybe (fromMaybe, listToMaybe)
import GHC.Float (double2Float)

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import qualified Convert.Number as ConvertNumber
import Data.Lisp (LispVal(LispNumber, LispFloat))

numberInBase :: ConvertNumber.NumberBase -> Parsec.Parser LispVal
numberInBase base =
  let
    baseCharParser :: Parsec.Parser String
    baseCharParser =
      Parsec.many1 (Parsec.oneOf (ConvertNumber.numberBaseCharSet base))

    convertParts :: String -> Maybe String -> LispVal
    convertParts big Nothing =
      LispNumber $ floor (fromMaybe 0 (ConvertNumber.readWholePart base big))
    convertParts big (Just small) =
      LispFloat $ double2Float (fromMaybe 0 (ConvertNumber.readParts base big small))
  in do
    whole <- baseCharParser
    maybeFraction <- Parsec.optionMaybe (Parsec.char '.' >> baseCharParser)
    return $ convertParts whole maybeFraction


decimalNumber :: Parsec.Parser LispVal
decimalNumber =
  numberInBase ConvertNumber.Decimal


hexadecimalNumber :: Parsec.Parser LispVal
hexadecimalNumber =
  numberInBase ConvertNumber.Hexadecimal


octalNumber :: Parsec.Parser LispVal
octalNumber =
  numberInBase ConvertNumber.Octal


binaryNumber :: Parsec.Parser LispVal
binaryNumber =
  numberInBase ConvertNumber.Binary


numberWithBasePrefix :: Parsec.Parser LispVal
numberWithBasePrefix =
  ((Parsec.char 'b') >> binaryNumber) <|>
  ((Parsec.char 'o') >> octalNumber) <|>
  ((Parsec.char 'd') >> decimalNumber) <|>
  ((Parsec.char 'x') >> hexadecimalNumber)

