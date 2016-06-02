module Main where

import Data.Functor (($>))
import Control.Monad (liftM)
import Data.Maybe (fromMaybe, listToMaybe)
import Numeric (readHex, readOct, readInt)
import Data.Char (digitToInt)
import Debug.Trace (trace)
import GHC.Float (double2Float)

import qualified System.Environment as SysEnv
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

import qualified Parse.Number as ParseNumber

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispFloat Float
             | LispString String
             | LispChar Char
             | LispBool Bool


main :: IO ()
main = do
  args <- SysEnv.getArgs
  putStrLn $ readExpr (args !! 0)


symbol :: Parsec.Parser Char
symbol = Parsec.oneOf "!$%&|*+-/:<=?>@^_~#"


fractionalPart :: Parsec.Parser String
fractionalPart = (Parsec.char '.' >> Parsec.many1 Parsec.digit)


spaces :: Parsec.Parser ()
spaces = Parsec.skipMany1 Parsec.space


escapedChar :: Parsec.Parser Char
escapedChar =
  Parsec.char '\\' >> (
    Parsec.oneOf "\"\"" <|>
    (Parsec.char 'n' $> '\n') <|>
    (Parsec.char 'r' $> '\r') <|>
    (Parsec.char 't' $> '\t')
  )


parseString :: Parsec.Parser LispVal
parseString = do
  Parsec.char '"'
  x <- Parsec.many ((Parsec.noneOf "\"") <|> escapedChar)
  Parsec.char '"'
  return $ LispString x


parseAtom :: Parsec.Parser LispVal
parseAtom = do
  first <- Parsec.letter <|> symbol
  rest <- Parsec.many (Parsec.letter <|> Parsec.digit <|> symbol)
  let atom = [first] ++ rest
  return $ LispAtom atom


parseBaseNumber :: ParseNumber.NumberBase -> Parsec.Parser LispVal
parseBaseNumber base =
  let
    baseCharParser :: Parsec.Parser String
    baseCharParser =
      Parsec.many1 (Parsec.oneOf (ParseNumber.numberBaseCharSet base))

    convertParts :: String -> Maybe String -> LispVal
    convertParts big Nothing =
      LispNumber $ floor (fromMaybe 0 (ParseNumber.readWholePart base big))
    convertParts big (Just small) =
      LispFloat $ double2Float (fromMaybe 0 (ParseNumber.readParts base big small))
  in do
    whole <- baseCharParser
    maybeFraction <- Parsec.optionMaybe (Parsec.char '.' >> baseCharParser)
    return $ convertParts whole maybeFraction


parseDecimalNumber :: Parsec.Parser LispVal
parseDecimalNumber =
  parseBaseNumber ParseNumber.Decimal


parseHexNumber :: Parsec.Parser LispVal
parseHexNumber =
  parseBaseNumber ParseNumber.Hexadecimal


parseOctalNumber :: Parsec.Parser LispVal
parseOctalNumber =
  parseBaseNumber ParseNumber.Octal


parseBinaryNumber :: Parsec.Parser LispVal
parseBinaryNumber =
  parseBaseNumber ParseNumber.Binary


parseCharacter :: Parsec.Parser LispVal
parseCharacter =
  ((Parsec.string "space")   >> return (LispChar ' ')) <|>
  ((Parsec.string "newline") >> return (LispChar '\n')) <|>
  ((Parsec.char '(')         >> return (LispChar '(')) <|>
  ((Parsec.char ' ')         >> return (LispChar ' ')) <|>
  (Parsec.letter            >>= (return . LispChar))


-- Parses all the things that start with a "#"
parseHashSymbol :: Parsec.Parser LispVal
parseHashSymbol =
  Parsec.char '#' >>
  (
    -- Characters
    ((Parsec.char '\\') >> parseCharacter) <|>
    -- Binary
    ((Parsec.char 'b') >> parseBinaryNumber) <|>
    ---- Octal
    ((Parsec.char 'o') >> parseOctalNumber) <|>
    ---- Decimal
    ((Parsec.char 'd') >> parseDecimalNumber) <|>
    ---- Hexadecimal
    ((Parsec.char 'x') >> parseHexNumber) <|>
    ---- Booleans
    ((Parsec.char 't') >> return (LispBool True)) <|>
    ((Parsec.char 'f') >> return (LispBool False))
  )


parseExp :: Parsec.Parser LispVal
parseExp =
  parseString <|>
  parseHashSymbol <|>
  parseDecimalNumber <|>
  parseAtom


readExpr :: String -> String
readExpr input =
  case Parsec.parse (parseExp >> Parsec.eof) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
