module Diceprob.Bool (fromBool) where

fromBool :: Bool -> Integer
fromBool = fromIntegral . fromEnum
