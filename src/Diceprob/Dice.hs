{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

{-# Language FlexibleInstances #-}

module Diceprob.Dice (
  Dice,
  dn,
  mdn,
  mdn',
  dSeq,
  dValues,
  dProbabilities,
  dType,
  dKeep,
  uniformPMF,
  pmfEqual
) where

import Data.AEq ((~==))
import Data.List (foldl', foldl1', groupBy, sort)

import Diceprob.Bool (fromBool)
import Diceprob.Op

type PMF = [(Int, Double)]

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

dn :: Int -> Dice
dn n = Dice { pmf = uniformPMF [1..n] }

mdn :: Int -> Int -> [Dice]
mdn m n
  | m <= 0    = error "number of dice must be positive"
  | otherwise = replicate m (dn n)

mdn' :: Int -> Int -> Dice
mdn' m n = foldl1' (#+) $ mdn m n

dSeq :: [Int] -> Dice
dSeq seq' = Dice { pmf = uniformPMF seq' }

dValues :: Dice -> [Int]
dValues = map fst . pmf

dProbabilities :: Dice -> [Double]
dProbabilities = map snd . pmf

dType :: Dice -> Either Int [Int]
dType d
  | empty                          = Right []
  | not consecutive || not uniform = Right values
  | otherwise                      = Left numValues
    where values        = dValues d
          numValues     = length $ values
          probabilities = dProbabilities d
          empty         = values == []
          consecutive   = values == [1..numValues]
          uniform       = all (== head probabilities) (tail probabilities)

dKeep :: [Dice] -> [Int] -> Dice
dKeep ds keep = Dice { pmf = reducePMF . groupPMF $ pmfDiscarded }
  where discard xs   = map snd $ filter (\(i,_) -> i `elem` keep) $ zip [1..] xs
        pmfCross     = crossPMF . map pmf $ ds
        pmfDiscarded = map (\(xs,p) -> (sum . discard $ xs,p)) pmfCross

diceMap :: (Int -> Int) -> Dice -> Dice
diceMap f d = Dice { pmf = reducePMF . groupPMF $ pmfMapped }
  where pmfMapped = map (\(x,p) -> (f x,p)) $ pmf d

diceCombine :: (Int -> Int -> Int) -> Dice -> Dice -> Dice
diceCombine f d d' = Dice { pmf = reducePMF . groupPMF $ pmfsCombined }
  where pmfsCombined = [(f x y, px * py) | (x, px) <- pmf d, (y, py) <- pmf d']

reducePMF :: [PMF] -> PMF
reducePMF = map $ foldl' (\(_,p) (x,p') -> (x,p + p')) (0,0)

groupPMF :: PMF -> [PMF]
groupPMF = groupBy (\a b -> fst a == fst b) . sort

uniformPMF :: [Int] -> PMF
uniformPMF domain = zip domain (replicate n p)
  where n = length $ domain :: Int
        p = 1.0 / fromIntegral n :: Double

crossPMF :: [PMF] -> [([Int], Double)]
crossPMF pmfs = map collect $ sequence pmfs
  where collect comb = (reverse . sort . map fst $ comb, product . map snd $ comb)

pmfEqual :: PMF -> Dice -> Bool
pmfEqual pmf' d = (v == v') && (all id $ zipWith approxEq p p')
  where (v,p)         = unzip . pmf $ d
        (v',p')       = unzip pmf'
        approxEq f f' = abs (f - f') / f < 0.01
