module Eval where

import Control.Monad.Trans.State.Lazy (State, modify, get)

import Data.Text (Text)

import Grammar

-- XXX use map instead
type Eval = State [(Text, Integer)]

eval :: Stmt -> Eval ()
eval (Sequence stmts)      = mapM_ eval stmts
eval (AssignmentExpr expr) = evalAssignmentExpr expr

evalArithmeticExpr :: ArithmeticExpr -> Eval Integer
evalArithmeticExpr expr = case expr of
  Constant n -> return n
  Variable v -> do
    env <- get
    case lookup v env of
      Just n -> return n
      Nothing -> error $ "variable '" ++ show v ++ "' not defined"
  Negation e           -> negate <$> evalArithmeticExpr e
  Sum e1 e2            -> (+) <$> evalArithmeticExpr e1 <*> evalArithmeticExpr e2
  Subtraction e1 e2    -> (-) <$> evalArithmeticExpr e1 <*> evalArithmeticExpr e2
  Product e1 e2        -> (*) <$> evalArithmeticExpr e1 <*> evalArithmeticExpr e2
  Division e1 e2       -> div <$> evalArithmeticExpr e1 <*> evalArithmeticExpr e2
  Exponentiation e1 e2 -> (^) <$> evalArithmeticExpr e1 <*> evalArithmeticExpr e2

evalAssignmentExpr :: AssignmentExpr -> Eval ()
evalAssignmentExpr (Assignment var expr) = do
  val <- evalArithmeticExpr expr
  modify ((var, val):)
