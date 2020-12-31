{-# OPTIONS_GHC -Wall -fno-cse #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)

import Test.Hspec

import Text.Megaparsec (runParser)

import Diceprob.AST (debugAST)
import Diceprob.Dice
import Diceprob.Eval
import Diceprob.Parser

testEval :: Parser a -> (a -> Eval b) -> [String] -> b
testEval parser eval' input = case ast of
    Left parseError -> error . debugAST $ parseError
    Right ast'      -> eval eval' ast'
  where ast = runParser parser "input.ad" (fromString . unlines $ input)

testStmt :: [String] -> [Output]
testStmt = testEval stmt evalStmt

testDiceExpr :: [String] -> Dice
testDiceExpr = testEval diceExpr evalDiceExpr

(?) :: (Show a, Eq a) => a -> a -> Expectation
(?) = shouldBe

main :: IO ()
main = hspec $ do
  describe "evalDiceExpr" $ do
    it "performs arithmetic" $ do
      testDiceExpr ["1+2"] ? 3
      testDiceExpr ["3-2"] ? 1
      testDiceExpr ["2*3"] ? 6
      testDiceExpr ["6/3"] ? 2
      testDiceExpr ["5/3"] ? 1
      testDiceExpr ["2^3"] ? 8
    it "checks conditions" $ do
      testDiceExpr ["2=2"]  ? 1
      testDiceExpr ["1=2"]  ? 0
      testDiceExpr ["1!=2"] ? 1
      testDiceExpr ["2!=2"] ? 0
      testDiceExpr ["1<2"]  ? 1
      testDiceExpr ["2<2"]  ? 0
      testDiceExpr ["3<2"]  ? 0
      testDiceExpr ["3>2"]  ? 1
      testDiceExpr ["2>2"]  ? 0
      testDiceExpr ["1>2"]  ? 0
      testDiceExpr ["2>=2"] ? 1
      testDiceExpr ["3>=2"] ? 1
      testDiceExpr ["1>=2"] ? 0
      testDiceExpr ["1<=2"] ? 1
      testDiceExpr ["2<=2"] ? 1
      testDiceExpr ["3<=2"] ? 0
      testDiceExpr ["1&1"]  ? 1
      testDiceExpr ["1&0"]  ? 0
      testDiceExpr ["0&1"]  ? 0
      testDiceExpr ["0&0"]  ? 0
      testDiceExpr ["1|1"]  ? 1
      testDiceExpr ["1|0"]  ? 1
      testDiceExpr ["0|1"]  ? 1
      testDiceExpr ["0|0"]  ? 0
      testDiceExpr ["!0"]   ? 1
      testDiceExpr ["!1"]   ? 0
      testDiceExpr ["!2"]   ? 0
    it "assigns and evaluates variables" $ do
      testStmt [
          "X: 3",
          "output X named \"It's a 3!\"",
          "output X + 1 named \"It's a 4!\"",
          "X: X * 2",
          "output X named \"It's a 6!\""
        ] ? [
          (3, Just "It's a 3!"),
          (4, Just "It's a 4!"),
          (6, Just "It's a 6!")
        ]
    it "modifies dice" $ do
      testStmt [
          "output d6 named \"1, 2, 3, 4, 5, 6\"",
          "output d6 + 10 named \"11, 12, 13, 14, 15, 16\"",
          "output d6 / 2 named \"0, 1, 1, 2, 2, 3\"",
          "output d6 > 3 named \"0, 0, 0, 1, 1, 1\""
        ] ? [
          (dn 6, Just "1, 2, 3, 4, 5, 6"),
          (dn 6 + 10, Just "11, 12, 13, 14, 15, 16"),
          (dn 6 / 2, Just "0, 1, 1, 2, 2, 3"),
          (diceGreater (dn 6) 3, Just "0, 0, 0, 1, 1, 1")
        ]
      dicePMFEqual (diceUniformPMF [1..6]) (dn 6)               ? True
      dicePMFEqual (diceUniformPMF [11..16]) (dn 6 + 10)        ? True
      dicePMFEqual [(0,1/6),(1,1/3),(2,1/3),(3,1/6)] (dn 6 / 2) ? True
      dicePMFEqual [(0,1/2),(1,1/2)] (diceGreater (dn 6) 3)     ? True
    it "combines dice" $ do
      testStmt [
          "output d2 + d3 named \"2, 3, 4, 3, 4, 5\"",
          "output d2 * d3 named \"1, 2, 3, 2, 4, 6\"",
          "output d2 / d3 named \"1, 0, 0, 2, 1, 0\"",
          "output d2 > d3 named \"0, 0, 0, 1, 0, 0\""
        ] ? [
          (dn 2 + dn 3, Just "2, 3, 4, 3, 4, 5"),
          (dn 2 * dn 3, Just "1, 2, 3, 2, 4, 6"),
          (dn 2 / dn 3, Just "1, 0, 0, 2, 1, 0"),
          (diceGreater (dn 2) (dn 3), Just "0, 0, 0, 1, 0, 0")
        ]
      dicePMFEqual [(2,1/6),(3,1/3),(4,1/3),(5,1/6)] (dn 2 + dn 3)         ? True
      dicePMFEqual [(1,1/6),(2,1/3),(3,1/6),(4,1/6),(6,1/6)] (dn 2 * dn 3) ? True
      dicePMFEqual [(0,1/2),(1,1/3),(2,1/6)] (dn 2 / dn 3)                 ? True
      dicePMFEqual [(0,5/6),(1,1/6)] (diceGreater (dn 2) (dn 3))           ? True
    it "collects dice" $ do
      testStmt [
          "output 2d2 named \"2, 3, 3, 4\"",
          "output 2d2 + 10 named \"12, 13, 13, 14\"",
          "output 2d2 > d3 named \"1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1\""
        ] ? [
          (dn 2 + dn 2, Just "2, 3, 3, 4"),
          (dn 2 + dn 2 + 10, Just "12, 13, 13, 14"),
          (diceGreater (dn 2 + dn 2) (dn 3), Just "1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1")
        ]
