module Main where

import qualified System.Environment as SysEnv
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Parsers.List as ParseList

main :: IO ()
main = do
  args <- SysEnv.getArgs
  putStrLn $ readExpr (args !! 0)


readExpr :: String -> String
readExpr input =
  case Parsec.parse (ParseList.expression >> Parsec.eof) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
