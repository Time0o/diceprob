module Grammar where

import Data.Text (Text)

data Stmt = Sequence [Stmt]
          | AssignmentExpr AssignmentExpr
          | OutputExpr OutputExpr
          deriving (Eq, Ord, Show)

data AssignmentExpr = Assignment Text IntegerExpr
                    deriving (Eq, Ord, Show)

data OutputExpr = Output IntegerExpr (Maybe Text)
                deriving (Eq, Ord, Show)

data IntegerExpr = Constant Integer
                 | Variable Text
                 | IntegerNegation IntegerExpr
                 | Sum IntegerExpr IntegerExpr
                 | Subtraction IntegerExpr IntegerExpr
                 | Product IntegerExpr IntegerExpr
                 | Division IntegerExpr IntegerExpr
                 | Exponentiation IntegerExpr IntegerExpr
                 | Equal IntegerExpr IntegerExpr
                 | NotEqual IntegerExpr IntegerExpr
                 | Smaller IntegerExpr IntegerExpr
                 | Greater IntegerExpr IntegerExpr
                 | AtLeast IntegerExpr IntegerExpr
                 | AtMost IntegerExpr IntegerExpr
                 | LogicalNegation IntegerExpr
                 | LogicalAnd IntegerExpr IntegerExpr
                 | LogicalOr IntegerExpr IntegerExpr
                 deriving (Eq, Ord, Show)
