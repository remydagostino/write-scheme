module Data.Lisp.Eval (eval) where

import qualified Data.Lisp.Num as LNum
import qualified Data.Lisp.String as LString
import qualified Data.Lisp.Atom as LAtom

import Data.Lisp
  ( LispNum(LispFloat, LispInt)
  , LispVal(..)
  )

eval :: LispVal -> LispVal
eval val =
  case val of
    (LispString _) -> val
    (LispNumber _) -> val
    (LispBool _) -> val
    (LispList [LispAtom "quote", tail]) -> tail
    (LispList (LispAtom func : args)) -> apply func $ map eval args


apply :: String -> [LispVal] -> LispVal
apply funcName args =
  let
    maybeFunc :: Maybe ([LispVal] -> LispVal)
    maybeFunc = lookup funcName primatives
  in
    case maybeFunc of
      Just func -> func args
      Nothing -> LispBool False


primatives :: [(String, [LispVal] -> LispVal)]
primatives =
  [("+", LNum.numericBinOp (+))
  ,("-", LNum.numericBinOp (-))
  ,("*", LNum.numericBinOp (*))
  ,("/", LNum.floatBinOp (/))
  ,("mod", LNum.integerBinOp mod)
  ,("quotient", LNum.integerBinOp quot)
  ,("remainder", LNum.integerBinOp rem)
  ,("string?", LString.isLispString)
  ,("number?", LNum.isLispNumber)
  ,("symbol?", LAtom.isLispAtom)
  ]


