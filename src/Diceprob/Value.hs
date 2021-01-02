{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

{-# LANGUAGE RankNTypes #-}

module Diceprob.Value where

import Prelude hiding (toInteger)

import Data.List (foldl1')

import Diceprob.Dice (Dice, dSeq, dValues)
import Diceprob.Integer
import Diceprob.Op

data Value = Integer Integer
           | Sequence [Integer]
           | Dice Dice
           | DiceCollection [Dice]
           deriving (Eq, Show)

valueUnaryOp :: (forall a. Op a => a -> a) -> (Value -> Value)
valueUnaryOp op v = case v of
  (Integer x)        -> Integer $ op x
  (Sequence _)       -> Integer $ op (valueToInteger v)
  (Dice x)           -> Dice    $ op x
  (DiceCollection _) -> Dice    $ op (valueToDice v)

valueBinaryOp :: (forall a. Op a => a -> a -> a) -> (Value -> Value -> Value)
valueBinaryOp op v v' = case (v, v') of
  (Dice x, _)           -> Dice    $ op x (valueToDice v')
  (_, Dice x)           -> Dice    $ op (valueToDice v) x
  (DiceCollection _, _) -> Dice    $ op (valueToDice v) (valueToDice v')
  (_, DiceCollection _) -> Dice    $ op (valueToDice v) (valueToDice v')
  (x, y)                -> Integer $ op (valueToInteger x) (valueToInteger y)

valueToInteger :: Value -> Integer
valueToInteger v = case v of
  Integer x  -> x
  Sequence x -> sum x
  _          -> undefined

valueToSequence :: Value -> [Integer]
valueToSequence v = case v of
  Integer x        -> [x]
  Sequence x       -> x
  Dice x           -> dValues x
  DiceCollection _ -> dValues . valueToDice $ v

valueToDice :: Value -> Dice
valueToDice v = case v of
  Integer x        -> dSeq [x]
  Sequence x       -> dSeq x
  Dice x           -> x
  DiceCollection x -> foldl1' (#+) x
