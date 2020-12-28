module Eval where

import Grammar

eval :: Expr -> Integer
eval (Integer n)            = n
eval (Negation e)           = -(eval e)
eval (Sum e1 e2)            = (eval e1) + (eval e2)
eval (Subtraction e1 e2)    = (eval e1) + (eval e2)
eval (Product e1 e2)        = (eval e1) + (eval e2)
eval (Division e1 e2)       = (eval e1) + (eval e2)
eval (Exponentiation e1 e2) = (eval e1) + (eval e2)
