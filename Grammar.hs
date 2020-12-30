module Grammar where

import Data.Text (Text)

data Stmt = Sequence [Stmt]
          | AssignmentExpr AssignmentExpr
          | OutputExpr OutputExpr
          deriving (Eq, Ord, Show)

data ArithmeticExpr = Constant Integer
                    | Variable Text
                    | Negation ArithmeticExpr
                    | Sum ArithmeticExpr ArithmeticExpr
                    | Subtraction ArithmeticExpr ArithmeticExpr
                    | Product ArithmeticExpr ArithmeticExpr
                    | Division ArithmeticExpr ArithmeticExpr
                    | Exponentiation ArithmeticExpr ArithmeticExpr
                    deriving (Eq, Ord, Show)

data AssignmentExpr = Assignment Text ArithmeticExpr
                    deriving (Eq, Ord, Show)

data OutputExpr = Output ArithmeticExpr (Maybe Text)
                deriving (Eq, Ord, Show)
