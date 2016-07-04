module Data.Lisp.String (isLispString) where

import Data.Lisp (LispVal(LispString, LispBool))

isLispString :: [LispVal] -> LispVal
isLispString [(LispString _)] = LispBool True
isLispString _ = LispBool False

