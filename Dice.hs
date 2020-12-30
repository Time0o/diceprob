{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

{-# Language FlexibleInstances #-}

module Dice where

import Data.List (groupBy, sort)

data Dice = Dice { pmf :: [(Integer, Double)] }
          deriving (Show)

diceCombine :: (Integer -> Integer -> Integer) -> Dice -> Dice -> Dice
diceCombine f d d' = Dice { pmf = reduce . group $ pmfsCombined }
  where pmfsCombined = [(f x y, px * py) | (x, px) <- pmf d, (y, py) <- pmf d']
        reduce       = map (foldl (\(_,p) (x,p') -> (x,p + p')) (0,0))
        group        = groupBy (\a b -> fst a == fst b) . sort

instance Num Dice where
  fromInteger n = Dice { pmf = [(n, 1.0)] }
  negate d      = Dice { pmf = map (\(x,p) -> (-x,p)) (pmf d) }
  (+)           = diceCombine (+)
  (*)           = diceCombine (*)

instance Fractional Dice where
  (/) = diceCombine div

dicePower :: Dice -> Dice -> Dice
dicePower = diceCombine (^)

fromBool :: Bool -> Integer
fromBool = fromIntegral . fromEnum

diceEqual    = diceCombine (\x y -> fromBool $ x == y)
diceNotEqual = diceCombine (\x y -> fromBool $ x /= y)
diceSmaller  = diceCombine (\x y -> fromBool $ x < y)
diceGreater  = diceCombine (\x y -> fromBool $ x > y)
diceAtLeast  = diceCombine (\x y -> fromBool $ x >= y)
diceAtMost   = diceCombine (\x y -> fromBool $ x <= y)
diceAnd      = diceCombine (\x y -> fromBool $ (x /= 0) && (y /= 0))
diceOr       = diceCombine (\x y -> fromBool $ (x /= 0) || (y /= 0))
diceNot d    = if pmf d == [(0,1.0)] then 1 else 0

dn :: Integer -> Dice
dn n = Dice { pmf = zip [1..n] (replicate (fromIntegral n) (1.0 / fromIntegral n)) }
