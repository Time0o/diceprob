module Grammar where

import Data.Text (Text)

import Dice (Dice)

data Stmt = Sequence [Stmt]
          | AssignmentExpr AssignmentExpr
          | OutputExpr OutputExpr
          deriving (Show)

data AssignmentExpr = Assignment Text DiceExpr
                    deriving (Show)

data OutputExpr = Output DiceExpr (Maybe Text)
                deriving (Show)

data DiceExpr = DiceLiteral Dice
              | IntegerLiteral Integer
              | Variable Text
              | Negation DiceExpr
              | Sum DiceExpr DiceExpr
              | Subtraction DiceExpr DiceExpr
              | Product DiceExpr DiceExpr
              | Division DiceExpr DiceExpr
              | Exponentiation DiceExpr DiceExpr
              | Equal DiceExpr DiceExpr
              | NotEqual DiceExpr DiceExpr
              | Smaller DiceExpr DiceExpr
              | Greater DiceExpr DiceExpr
              | AtLeast DiceExpr DiceExpr
              | AtMost DiceExpr DiceExpr
              | Not DiceExpr
              | And DiceExpr DiceExpr
              | Or DiceExpr DiceExpr
              deriving (Show)
