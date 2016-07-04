module Data.Lisp.Atom (isLispAtom) where

import Data.Lisp (LispVal(LispAtom, LispBool))

isLispAtom :: [LispVal] -> LispVal
isLispAtom [(LispAtom _)] = LispBool True
isLispAtom _ = LispBool False

