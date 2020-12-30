module Eval where

import Control.Monad.Trans.State.Lazy (State, modify, get, runState)

import Data.Maybe (catMaybes)
import Data.Text (Text)

import Grammar

-- XXX use map instead
type Eval = State [(Text, Integer)]

eval :: Stmt -> [Integer]
eval stmt = fst $ runState (eval' stmt) []

eval' :: Stmt -> Eval [Integer]
eval' (Sequence stmts) = catMaybes <$> mapM eval'' stmts

eval'' :: Stmt -> Eval (Maybe Integer)
eval'' (AssignmentExpr expr) = Nothing <$ evalAssignmentExpr expr
eval'' (OutputExpr expr)     = Just <$> evalOutputExpr expr

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

evalOutputExpr :: OutputExpr -> Eval Integer
evalOutputExpr (Output expr) = evalArithmeticExpr expr
