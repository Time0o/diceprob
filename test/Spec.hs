{-# OPTIONS_GHC -Wall -fno-cse #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)

import Test.Hspec

import Text.Megaparsec (runParser)

import Diceprob.AST (debugAST)
import Diceprob.Dice
import Diceprob.Eval
import Diceprob.Op
import Diceprob.Parser
import Diceprob.Value

testEval :: Parser a -> (a -> Eval b) -> [String] -> b
testEval parser eval' input = case ast of
    Left parseError -> error . debugAST $ parseError
    Right ast'      -> eval eval' ast'
  where ast = runParser parser "input.ad" (fromString . unlines $ input)

testStmt :: [String] -> [Output]
testStmt = testEval stmt evalStmt

testValueExpr :: [String] -> Value
testValueExpr = testEval valueExpr evalValueExpr

(?) :: (Show a, Eq a) => a -> a -> Expectation
(?) = shouldBe

main :: IO ()
main = hspec $ do
  describe "evalValueExpr" $ do
    it "performs arithmetic" $ do
      testValueExpr ["1+2"] ? Integer 3
      testValueExpr ["3-2"] ? Integer 1
      testValueExpr ["2*3"] ? Integer 6
      testValueExpr ["6/3"] ? Integer 2
      testValueExpr ["5/3"] ? Integer 1
      testValueExpr ["2^3"] ? Integer 8
    it "checks conditions" $ do
      testValueExpr ["2=2"]  ? Integer 1
      testValueExpr ["1=2"]  ? Integer 0
      testValueExpr ["1!=2"] ? Integer 1
      testValueExpr ["2!=2"] ? Integer 0
      testValueExpr ["1<2"]  ? Integer 1
      testValueExpr ["2<2"]  ? Integer 0
      testValueExpr ["3<2"]  ? Integer 0
      testValueExpr ["3>2"]  ? Integer 1
      testValueExpr ["2>2"]  ? Integer 0
      testValueExpr ["1>2"]  ? Integer 0
      testValueExpr ["2>=2"] ? Integer 1
      testValueExpr ["3>=2"] ? Integer 1
      testValueExpr ["1>=2"] ? Integer 0
      testValueExpr ["1<=2"] ? Integer 1
      testValueExpr ["2<=2"] ? Integer 1
      testValueExpr ["3<=2"] ? Integer 0
      testValueExpr ["1&1"]  ? Integer 1
      testValueExpr ["1&0"]  ? Integer 0
      testValueExpr ["0&1"]  ? Integer 0
      testValueExpr ["0&0"]  ? Integer 0
      testValueExpr ["1|1"]  ? Integer 1
      testValueExpr ["1|0"]  ? Integer 1
      testValueExpr ["0|1"]  ? Integer 1
      testValueExpr ["0|0"]  ? Integer 0
      testValueExpr ["!0"]   ? Integer 1
      testValueExpr ["!1"]   ? Integer 0
      testValueExpr ["!2"]   ? Integer 0
    it "assigns and evaluates variables" $ do
      testStmt [
          "X: 3",
          "output X named \"It's a 3!\"",
          "output X + 1 named \"It's a 4!\"",
          "X: X * 2",
          "output X named \"It's a 6!\""
        ] ? [
          (dSeq [3], Just "It's a 3!"),
          (dSeq [4], Just "It's a 4!"),
          (dSeq [6], Just "It's a 6!")
        ]
    it "modifies dice" $ do
      testStmt [
          "output d6 named \"1, 2, 3, 4, 5, 6\"",
          "output d6 + 10 named \"11, 12, 13, 14, 15, 16\"",
          "output d6 / 2 named \"0, 1, 1, 2, 2, 3\"",
          "output d6 > 3 named \"0, 0, 0, 1, 1, 1\""
        ] ? [
          (dn 6, Just "1, 2, 3, 4, 5, 6"),
          (dn 6 #+ dSeq [10], Just "11, 12, 13, 14, 15, 16"),
          (dn 6 #/ dSeq [2], Just "0, 1, 1, 2, 2, 3"),
          (dn 6 #> dSeq [3], Just "0, 0, 0, 1, 1, 1")
        ]
      pmfEqual (uniformPMF [1..6]) (dn 6)                           ? True
      pmfEqual (uniformPMF [11..16]) (dn 6 #+ dSeq [10])            ? True
      pmfEqual [(0,1/6),(1,1/3),(2,1/3),(3,1/6)] (dn 6 #/ dSeq [2]) ? True
      pmfEqual [(0,1/2),(1,1/2)] (dn 6 #> dSeq [3])                 ? True
    it "combines dice" $ do
      testStmt [
          "output d2 + d3 named \"2, 3, 4, 3, 4, 5\"",
          "output d2 * d3 named \"1, 2, 3, 2, 4, 6\"",
          "output d2 / d3 named \"1, 0, 0, 2, 1, 0\"",
          "output d2 > d3 named \"0, 0, 0, 1, 0, 0\""
        ] ? [
          (dn 2 #+ dn 3, Just "2, 3, 4, 3, 4, 5"),
          (dn 2 #* dn 3, Just "1, 2, 3, 2, 4, 6"),
          (dn 2 #/ dn 3, Just "1, 0, 0, 2, 1, 0"),
          (dn 2 #> dn 3, Just "0, 0, 0, 1, 0, 0")
        ]
      pmfEqual [(2,1/6),(3,1/3),(4,1/3),(5,1/6)] (dn 2 #+ dn 3)         ? True
      pmfEqual [(1,1/6),(2,1/3),(3,1/6),(4,1/6),(6,1/6)] (dn 2 #* dn 3) ? True
      pmfEqual [(0,1/2),(1,1/3),(2,1/6)] (dn 2 #/ dn 3)                 ? True
      pmfEqual [(0,5/6),(1,1/6)] (dn 2 #> dn 3)                         ? True
    it "collects dice" $ do
      testStmt [
          "output 2d2 named \"2, 3, 3, 4\"",
          "output 2d2 + 10 named \"12, 13, 13, 14\"",
          "output 2d2 > d3 named \"1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1\""
        ] ? [
          (dn 2 #+ dn 2, Just "2, 3, 3, 4"),
          (dn 2 #+ dn 2 #+ dSeq [10], Just "12, 13, 13, 14"),
          (dn 2 #+ dn 2 #> dn 3, Just "1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1")
        ]
    it "evaluates sequences" $ do
      testStmt [
          "output {1, 2, 3} named \"1, 2, 3\"",
          "output {1..3} named \"1, 2, 3\"",
          "output {1:4, 2, 3} named \"1, 1, 1, 1, 2, 3\"",
          "output {1..3:2} named \"1, 2, 3, 1, 2, 3\"",
          "output {{1,2,4}:2, 5, 6} named \"1, 2, 4, 1, 2, 4, 5, 6\"",
          "output {d4} named \"1, 2, 3, 4\"",
          "output {} named \"the empty sequence\""
        ] ? [
          (dSeq [1, 2, 3], Just "1, 2, 3"),
          (dSeq [1, 2, 3], Just "1, 2, 3"),
          (dSeq [1, 1, 1, 1, 2, 3], Just "1, 1, 1, 1, 2, 3"),
          (dSeq [1, 2, 3, 1, 2, 3], Just "1, 2, 3, 1, 2, 3"),
          (dSeq [1, 2, 4, 1, 2, 4, 5, 6], Just "1, 2, 4, 1, 2, 4, 5, 6"),
          (dSeq [1, 2, 3, 4], Just "1, 2, 3, 4"),
          (dSeq [], Just "the empty sequence")
        ]
