{-# OPTIONS_GHC -Wall #-}

module Diceprob.Grammar where

import Data.Text (Text)

import Diceprob.Dice (Dice)

data Stmt = Stmts [Stmt]
          | AssignmentExpr AssignmentExpr
          | OutputExpr OutputExpr
          deriving (Show)

data AssignmentExpr = Assignment Text ValueExpr
                    deriving (Show)

data OutputExpr = Output ValueExpr (Maybe Text)
                deriving (Show)

data ValueExpr = Literal Literal
               | Variable Text
               | Negation ValueExpr
               | Sum ValueExpr ValueExpr
               | Subtraction ValueExpr ValueExpr
               | Product ValueExpr ValueExpr
               | Division ValueExpr ValueExpr
               | Exponentiation ValueExpr ValueExpr
               | Equal ValueExpr ValueExpr
               | NotEqual ValueExpr ValueExpr
               | Smaller ValueExpr ValueExpr
               | Greater ValueExpr ValueExpr
               | AtLeast ValueExpr ValueExpr
               | AtMost ValueExpr ValueExpr
               | Not ValueExpr
               | And ValueExpr ValueExpr
               | Or ValueExpr ValueExpr
               deriving (Show)

data SequenceElement = Element ValueExpr
                     | Range ValueExpr ValueExpr
                     | Repeat ValueExpr ValueExpr
                     deriving (Show)

data Literal = IntegerLiteral Integer
             | SequenceLiteral [SequenceElement]
             | DiceLiteral Dice
             | DiceCollectionLiteral [Dice]
             deriving (Show)
