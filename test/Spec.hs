{-# OPTIONS_GHC -Wall -fno-cse #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)

import Test.Hspec

import Text.Megaparsec (runParser)

import Diceprob.AST (AST, debugAST)
import Diceprob.Dice
import Diceprob.Eval
import Diceprob.Op
import Diceprob.Parser
import Diceprob.Value

testAST :: Parser a -> [String] -> AST a
testAST parser = runParser parser "input.ad" . fromString . unlines

testEval :: (a -> Eval b) -> Parser a -> [String] -> b
testEval eval' parser input =
  let ast = testAST parser input
  in case ast of
    Left parseError -> error . debugAST $ parseError
    Right ast'      -> eval eval' ast'

testStmt :: [String] -> [Output]
testStmt = testEval evalStmt stmt

testValueExpr :: [String] -> Value
testValueExpr = testEval evalValueExpr valueExpr

(?) :: (Show a, Eq a) => a -> a -> Expectation
(?) = shouldBe

main :: IO ()
main = hspec $ do
  describe "diceprob" $ do
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
          (mdn' 2 2, Just "2, 3, 3, 4"),
          (mdn' 2 2 #+ dSeq [10], Just "12, 13, 13, 14"),
          (mdn' 2 2 #> dn 3, Just "1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1")
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
          (dSeq [1..3], Just "1, 2, 3"),
          (dSeq [1..3], Just "1, 2, 3"),
          (dSeq [1, 1, 1, 1, 2, 3], Just "1, 1, 1, 1, 2, 3"),
          (dSeq [1, 2, 3, 1, 2, 3], Just "1, 2, 3, 1, 2, 3"),
          (dSeq [1, 2, 4, 1, 2, 4, 5, 6], Just "1, 2, 4, 1, 2, 4, 5, 6"),
          (dSeq [1..4], Just "1, 2, 3, 4"),
          (dSeq [], Just "the empty sequence")
        ]
    it "compares sequences" $ do
      testValueExpr ["{1,2,3,4} < 3"]      ? Integer 2
      testValueExpr ["3 > {1,2,3,4}"]      ? Integer 2
      testValueExpr ["{1,2,3,4} < d20"]    ? Dice (dSeq [10] #< dn 20)
      testValueExpr ["d20 > {1,2,3,4}"]    ? Dice (dn 20 #> dSeq [10])
      testValueExpr ["{1,2,3} = {1,2,3}"]  ? Integer 1
      testValueExpr ["{1,2,3} != {1,2,3}"] ? Integer 0
      testValueExpr ["{1,2,3} = {1,2,4}"]  ? Integer 0
      testValueExpr ["{1,2,3} != {1,2,4}"] ? Integer 1
      testValueExpr ["{1,2,3} < {1,2,4}"]  ? Integer 1
      testValueExpr ["{1,2,3} > {1,2,4}"]  ? Integer 0
      testValueExpr ["{1,2,4} < {1,2,3}"]  ? Integer 0
      testValueExpr ["{1,2,4} > {1,2,3}"]  ? Integer 1
    it "performs introspection" $ do
      testValueExpr ["#{2,4,6}"] ? Integer 3
      testValueExpr ["#(3d6)"]   ? Integer 3
      testValueExpr ["#3d6"]     ? DiceCollection (mdn 1 6)
      testValueExpr ["#d6"]      ? Integer 1
      testValueExpr ["#123"]     ? Integer 3
    it "expands variables in strings" $ do
      testStmt [
          "X: 4",
          "output X named \"this is a [X]\"",
          "X: d4",
          "output X named \"this is a [X]\"",
          "X: 2d4",
          "output X named \"this is a [X]\"",
          "X: {1..4}",
          "output X named \"this is a [X]\""
        ] ? [
          (dSeq [4], Just "this is a 4"),
          (dn 4, Just "this is a d4"),
          (mdn' 2 4, Just "this is a 2d4"),
          (dSeq [1..4], Just "this is a {?}")
        ]
    it "executes loops" $ do
      testStmt [
          "loop N over {1..4} {",
          "  output Nd4 named \"[N]d4\"",
          "  output 1dN named \"1d[N]\"",
          "}"
        ] ? [
          (mdn' 1 4, Just "1d4"),
          (dn 1, Just "1d1"),
          (mdn' 2 4, Just "2d4"),
          (dn 2, Just "1d2"),
          (mdn' 3 4, Just "3d4"),
          (dn 3, Just "1d3"),
          (mdn' 4 4, Just "4d4"),
          (dn 4, Just "1d4")
        ]
    it "executes branches" $ do
      testStmt [
          "X: 1",
          "if X = 1 {",
          "  output 1",
          "} else {",
          "  output 1d6",
          "}",
          "X: 0",
          "if X = 1 {",
          "  output 1",
          "} else {",
          "  output 1d6",
          "}"
        ] ? [
          (dSeq [1], Nothing),
          (dn 6, Nothing)
        ]
