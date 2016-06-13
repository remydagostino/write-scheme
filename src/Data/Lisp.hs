module Data.Lisp (LispVal(..), showVal) where

data LispVal = LispAtom String
             | LispList [LispVal]
             | LispDottedList [LispVal] LispVal
             | LispNumber Integer
             | LispFloat Float
             | LispString String
             | LispChar Char
             | LispBool Bool

