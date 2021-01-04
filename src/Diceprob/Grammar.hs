{-# OPTIONS_GHC -Wall #-}

module Diceprob.Grammar where

import Data.Text (Text)

data Stmt = Stmts [Stmt]
          | AssignmentExpr AssignmentExpr
          | LoopExpr LoopExpr
          | BranchExpr BranchExpr
          | OutputExpr OutputExpr
          deriving (Show)

data AssignmentExpr = Assignment Text ValueExpr
                    deriving (Show)

data LoopExpr = Loop Text ValueExpr Stmt
              deriving (Show)

data BranchExpr = Branch ValueExpr Stmt (Maybe Stmt)
                deriving (Show)

data OutputExpr = NamedOutput ValueExpr [Either Text Text]
                | UnnamedOutput ValueExpr
                deriving (Show)

data Range = Range ValueExpr ValueExpr
           deriving (Show)

data Repeat = RepeatValue ValueExpr ValueExpr
            | RepeatRange Range ValueExpr
            deriving (Show)

data SequenceElement = SequenceValue ValueExpr
                     | SequenceRange Range
                     | SequenceRepeat Repeat
                     deriving (Show)

type Sequence = [SequenceElement]

data ValueExpr = IntegerLiteral Integer
               | SequenceLiteral Sequence
               | DiceLiteral ValueExpr
               | DiceCollectionLiteral ValueExpr ValueExpr
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
