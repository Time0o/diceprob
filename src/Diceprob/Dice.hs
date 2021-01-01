{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

{-# Language FlexibleInstances #-}

module Diceprob.Dice (
  Dice,
  dn,
  dSeq,
  uniformPMF,
  pmfEqual
) where

import Data.AEq ((~==))
import Data.List (groupBy, sort)

import Diceprob.Bool (fromBool)
import Diceprob.Op

type PMF = [(Integer, Double)]

data Dice = Dice { pmf :: PMF }
          deriving (Eq, Show)

instance Op Dice where
  (#-)   = diceMap negate
  (#+)   = diceCombine (+)
  (#--)  = diceCombine (-)
  (#*)   = diceCombine (*)
  (#/)   = diceCombine (div)
  (#^)   = diceCombine (^)
  (#=)   = diceCombine (\d d' -> fromBool $ d == d')
  (#!=)  = diceCombine (\d d' -> fromBool $ d /= d')
  (#<)   = diceCombine (\d d' -> fromBool $ d < d')
  (#>)   = diceCombine (\d d' -> fromBool $ d > d')
  (#<=)  = diceCombine (\d d' -> fromBool $ d <= d')
  (#>=)  = diceCombine (\d d' -> fromBool $ d >= d')
  (#!)   = diceMap (\d -> fromBool $ d == 0)
  (#&)   = diceCombine (\x y -> fromBool $ (x /= 0) && (y /= 0))
  (#|)   = diceCombine (\x y -> fromBool $ (x /= 0) || (y /= 0))

dn :: Integer -> Dice
dn n = Dice { pmf = uniformPMF [1..n] }

dSeq :: [Integer] -> Dice
dSeq seq' = Dice { pmf = uniformPMF seq' }

diceMap :: (Integer -> Integer) -> Dice -> Dice
diceMap f d = Dice { pmf = reducePMF . groupPMF $ pmfMapped }
  where pmfMapped = map (\(x,p) -> (f x,p)) $ pmf d

diceCombine :: (Integer -> Integer -> Integer) -> Dice -> Dice -> Dice
diceCombine f d d' = Dice { pmf = reducePMF . groupPMF $ pmfsCombined }
  where pmfsCombined = [(f x y, px * py) | (x, px) <- pmf d, (y, py) <- pmf d']

reducePMF :: [PMF] -> PMF
reducePMF = map $ foldl (\(_,p) (x,p') -> (x,p + p')) (0,0)

groupPMF :: PMF -> [PMF]
groupPMF = groupBy (\a b -> fst a == fst b) . sort

uniformPMF :: [Integer] -> PMF
uniformPMF domain = zip domain (replicate n p)
  where n = fromIntegral . length $ domain :: Int
        p = 1.0 / fromIntegral n :: Double

pmfEqual :: PMF -> Dice -> Bool
pmfEqual pmf' d = (v == v') && (all id $ zipWith (~==) p p')
  where (v,p)   = unzip . pmf $ d
        (v',p') = unzip pmf'