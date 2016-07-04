module Data.Lisp.Num
  ( LispNum(LispFloat, LispInt)
  , isLispNumber
  , numericBinOp
  , floatBinOp
  , integerBinOp
  ) where

import Data.Lisp
  ( LispVal(LispNumber, LispBool, LispList)
  , LispNum(LispFloat, LispInt)
  )


isLispNumber :: [LispVal] -> LispVal
isLispNumber [(LispNumber _)] = LispBool True
isLispNumber _ = LispBool False


numericBinOp :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> LispVal
numericBinOp op params =
  let
    numericParams = map coerceToLispNum params
    result = foldl1 op numericParams
  in
    LispNumber result


coerceToLispNum :: LispVal -> LispNum
coerceToLispNum val =
  case val of
    (LispNumber num) -> num
    (LispList [num]) -> coerceToLispNum num
    _ -> undefined


floatBinOp :: (Float -> Float -> Float) -> [LispVal] -> LispVal
floatBinOp op params =
  let
    numericParams = map coerceToFloat params
    result = foldl1 op numericParams
  in
    LispNumber (LispFloat result)


coerceToFloat :: LispVal -> Float
coerceToFloat val =
  case val of
    (LispNumber (LispFloat f)) -> f
    (LispNumber (LispInt i)) -> fromIntegral i
    (LispList [num]) -> coerceToFloat num
    _ -> undefined


integerBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
integerBinOp op params =
  let
    numericParams = map coerceToInteger params
    result = foldl1 op numericParams
  in
    LispNumber (LispInt result)


coerceToInteger :: LispVal -> Integer
coerceToInteger val =
  case val of
    (LispNumber (LispFloat f)) -> floor f
    (LispNumber (LispInt i)) -> i
    (LispList [num]) -> coerceToInteger num
    _ -> undefined
