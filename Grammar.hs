module Grammar where

data Expr = Integer Integer
          | Negation Expr
          | Sum Expr Expr
          | Subtraction Expr Expr
          | Product Expr Expr
          | Division Expr Expr
          | Exponentiation Expr Expr
          deriving (Eq, Ord, Show)
