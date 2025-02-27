{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Diceprob.Value where

import Prelude hiding (toInteger)

import Data.Char (digitToInt)
import Data.List (foldl1')
import Data.Text (Text)

import Diceprob.Dice (Dice, dSeq, dValues, dType, dKeep)
import Diceprob.Error
import Diceprob.Integer
import Diceprob.Op
import Diceprob.Text

data Value = Integer Int
           | Sequence [Int]
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

valueSequenceBinaryOp :: (forall a. Op a => a -> a -> a) -> (Value -> Value -> Value)
valueSequenceBinaryOp op s s' = case (s, s') of
  (Sequence x, Sequence y) -> Integer $ head $ op x y
  (Integer x, Sequence y)  -> Integer $ sum . map (op x) $ y
  (Sequence x, Integer y)  -> Integer $ sum . map (flip op y) $ x
  (x, Sequence y)          -> Dice    $ op (valueToDice x) (dSeq [sum y])
  (Sequence x, y)          -> Dice    $ op (dSeq [sum x]) (valueToDice y)
  _                        -> valueBinaryOp op s s'

valueToInteger :: Value -> Int
valueToInteger v = case v of
  Integer x  -> x
  Sequence x -> sum x
  _          -> undefined

valueToSequence :: Value -> [Int]
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

valueToText :: Value -> Text
valueToText v = case v of
  Integer x  -> textShow $ x
  Sequence _ -> "{?}"
  Dice x -> case dType x of
    Left n   -> "d" <> textShow n
    Right [] -> "d{}"
    Right _  -> "d{?}"
  DiceCollection x -> m <> d
    where m = textShow . length $ x
          d = valueToText . Dice . head $ x

valueLength :: Value -> Int
valueLength v = case v of
  Integer x        -> length . show $ x
  Sequence x       -> length x
  Dice _           -> 1
  DiceCollection x -> length x

valueAccess :: Value -> Value -> Value
valueAccess vi v = case (vi, v) of
  (Integer  i, Integer x)        -> Integer $ digitAt x i
  (Integer  i, Sequence x)       -> Integer $ sequenceAt x i
  (Integer  i, Dice x)           -> Dice    $ diceAt x i
  (Integer  i, DiceCollection x) -> Dice    $ dKeep x [i]
  (Sequence i, Integer x)        -> Integer $ sum . map (digitAt x) $ i
  (Sequence i, Sequence x)       -> Integer $ sum . map (sequenceAt x) $ i
  (Sequence i, Dice x)           -> Dice    $ foldl1' (#+) . map (diceAt x) $ i
  (Sequence i, DiceCollection x) -> Dice    $ dKeep x i
  _                              -> valueError err_access vi

digitAt :: Int -> Int -> Int
digitAt n i = let t = show n
              in if i <= 0 || i > length t
                 then 0
                 else digitToInt $ t !! (i - 1)

sequenceAt :: [Int] -> Int -> Int
sequenceAt s i = if i <= 0 || i > length s
                 then 0
                 else s !! (i - 1)

diceAt :: Dice -> Int -> Dice
diceAt d i = if i == 1 then d else dSeq [0]

valueError :: Text -> Value -> a
valueError msg v = errorf msg (valueToText v)
