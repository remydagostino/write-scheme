module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Parsers.List as ParseList
import Data.Lisp.Eval (eval)
import Data.Lisp (LispVal(LispString))

main :: IO ()
main =
  let
    run :: [String] -> IO ()
    run [lispInput] = print . eval . readExpr $ lispInput
    run _ = putStrLn "Bad Input" >> exitFailure
  in
    getArgs >>= run


schemeParser :: Parsec.Parser LispVal
schemeParser = do
  val <- ParseList.expression
  Parsec.eof
  return val


readExpr :: String -> LispVal
readExpr input =
  case Parsec.parse schemeParser "lisp" input of
    Left err -> LispString $ "No match: " ++ show err
    Right val -> val
