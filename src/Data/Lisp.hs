module Data.Lisp (LispVal(..), LispNum(..), showVal) where

data LispNum
  = LispFloat Float
  | LispInt Integer

data LispVal
  = LispAtom String
  | LispNumber LispNum
  | LispString String
  | LispChar Char
  | LispBool Bool
  | LispList [LispVal]
  | LispDottedList [LispVal] LispVal

instance Show LispVal where show = showVal

instance Show LispNum where
  show (LispFloat x) = show x
  show (LispInt x) = show x

instance Num LispNum where
  (LispInt a) + (LispInt b) = LispInt $ a + b
  (LispFloat a) + (LispFloat b) = LispFloat $ a + b
  a + b = (toLispFloat a) + (toLispFloat b)

  (LispInt a) * (LispInt b) = LispInt $ a * b
  (LispFloat a) * (LispFloat b) = LispFloat $ a * b
  a * b = (toLispFloat a) * (toLispFloat b)

  abs (LispInt a) = LispInt (abs a)
  abs (LispFloat a) = LispFloat (abs a)

  signum (LispInt a) = LispInt (signum a)
  signum (LispFloat a) = LispFloat (signum a)

  negate (LispInt a) = LispInt (negate a)
  negate (LispFloat a) = LispFloat (negate a)

  fromInteger int = LispInt int


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


showVal :: LispVal -> String
showVal val =
  case val of
    (LispAtom name) -> name
    (LispNumber num) -> show num
    (LispString str) -> "\"" ++ str ++ "\""
    (LispChar char) -> '\\' : char : ""
    (LispBool True) -> "#t"
    (LispBool False) -> "#f"
    (LispList contents) -> "(" ++ unwordsList contents ++ ")"
    (LispDottedList head tail) -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++  ")"


toLispFloat :: LispNum -> LispNum
toLispFloat a =
  case a of
    (LispFloat _) -> a
    (LispInt i) -> LispFloat $ fromIntegral i

