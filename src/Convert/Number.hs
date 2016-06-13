module Convert.Number
  ( NumberBase(Hexadecimal, Decimal, Octal, Binary)
  , readWholePart
  , readFractionalPart
  , readParts
  , numberBaseCharSet
  , numberBaseRoot
  ) where

import qualified Data.List as List
import Control.Applicative (liftA2)

data NumberBase = Hexadecimal | Decimal | Octal | Binary

numberBaseCharSet :: NumberBase -> String
numberBaseCharSet Hexadecimal = "0123456789abcdef"
numberBaseCharSet Decimal = "0123456789"
numberBaseCharSet Octal = "01234567"
numberBaseCharSet Binary = "01"


numberBaseRoot :: NumberBase -> Double
numberBaseRoot Hexadecimal = 16
numberBaseRoot Decimal = 10
numberBaseRoot Octal = 8
numberBaseRoot Binary = 2


baseSet :: Double -> [Double]
baseSet base =
  (map (\a -> base ^ a) [0..])


fractionalBaseSet :: Double -> [Double]
fractionalBaseSet base =
  (map (\a -> 1 / (base ^ a)) [1..])


readInBase :: String -> [Double] -> String -> Maybe Double
readInBase charSet baseSet input =
  let
    inputValues :: [Maybe Int]
    inputValues =
      map (\x -> List.elemIndex x charSet) input

    combinedWithBase :: [(Maybe Int, Double)]
    combinedWithBase =
      zip inputValues baseSet

    accumulatePairs :: Maybe Double -> (Maybe Int, Double) -> Maybe Double
    accumulatePairs (Just acc) ((Just value), base) =
      Just (acc + ((fromIntegral value) * base))
    accumulatePairs _ _ =
      Nothing
  in
    foldl accumulatePairs (Just 0) combinedWithBase


readParts :: NumberBase -> String -> String -> Maybe Double
readParts base wholePart fractionalPart =
  liftA2 (+) (readWholePart base wholePart) (readFractionalPart base fractionalPart)


readWholePart :: NumberBase -> String -> Maybe Double
readWholePart base input =
  readInBase (numberBaseCharSet base) (baseSet $ numberBaseRoot base) (reverse input)


readFractionalPart :: NumberBase -> String -> Maybe Double
readFractionalPart base input =
  readInBase (numberBaseCharSet base) (fractionalBaseSet $ numberBaseRoot base) input
