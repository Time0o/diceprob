{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

{-# Language FlexibleInstances #-}

module Diceprob.Dice (
  Dice,
  dn,
  diceUniformPMF,
  dicePMFEqual,
  dicePower,
  diceEqual,
  diceNotEqual,
  diceSmaller,
  diceGreater,
  diceAtLeast,
  diceAtMost,
  diceAnd,
  diceOr,
  diceNot
) where

import Data.AEq ((~==))
import Data.List (groupBy, sort)

import Diceprob.Bool (fromBool)

data Dice = Dice { pmf :: [(Integer, Double)] }
          deriving (Eq, Show)

instance Num Dice where
  fromInteger n = Dice { pmf = [(n, 1.0)] }
  negate d      = Dice { pmf = map (\(x,p) -> (-x,p)) (pmf d) }
  (+)           = diceCombine (+)
  (*)           = diceCombine (*)

instance Fractional Dice where
  (/) = diceCombine div

diceCombine :: (Integer -> Integer -> Integer) -> Dice -> Dice -> Dice
diceCombine f d d' = Dice { pmf = reduce . group $ pmfsCombined }
  where pmfsCombined = [(f x y, px * py) | (x, px) <- pmf d, (y, py) <- pmf d']
        reduce       = map (foldl (\(_,p) (x,p') -> (x,p + p')) (0,0))
        group        = groupBy (\a b -> fst a == fst b) . sort

dn :: Integer -> Dice
dn n = Dice { pmf = diceUniformPMF [1..n] }

diceUniformPMF :: [Integer] -> [(Integer,Double)]
diceUniformPMF domain = zip domain (replicate n p)
  where n = fromIntegral . length $ domain :: Int
        p = 1.0 / fromIntegral n :: Double

dicePMFEqual :: [(Integer, Double)] -> Dice -> Bool
dicePMFEqual pmf' d = (v == v') && (all id $ zipWith (~==) p p')
  where (v,p)   = unzip . pmf $ d
        (v',p') = unzip pmf'

dicePower :: Dice -> Dice -> Dice
dicePower = diceCombine (^)

diceEqual, diceNotEqual :: Dice -> Dice -> Dice
diceEqual    = diceCombine (\x y -> fromBool $ x == y)
diceNotEqual = diceCombine (\x y -> fromBool $ x /= y)

diceSmaller, diceGreater, diceAtLeast, diceAtMost :: Dice -> Dice -> Dice
diceSmaller = diceCombine (\x y -> fromBool $ x < y)
diceGreater = diceCombine (\x y -> fromBool $ x > y)
diceAtLeast = diceCombine (\x y -> fromBool $ x >= y)
diceAtMost  = diceCombine (\x y -> fromBool $ x <= y)

diceAnd, diceOr :: Dice -> Dice -> Dice
diceAnd = diceCombine (\x y -> fromBool $ (x /= 0) && (y /= 0))
diceOr  = diceCombine (\x y -> fromBool $ (x /= 0) || (y /= 0))

diceNot :: Dice -> Dice
diceNot d = if pmf d == [(0,1.0)] then 1 else 0
