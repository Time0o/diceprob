module Eval where

import Prelude hiding (lookup)

import Control.Monad.Trans.State.Lazy (State, modify, get, runState)

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, empty, insert, lookup)

import Bool
import Grammar

type Output = (Integer, Maybe Text)

type Eval = State (HashMap Text Integer)

eval :: Stmt -> [Output]
eval stmt = fst $ runState (eval' stmt) empty

eval' :: Stmt -> Eval [Output]
eval' (Sequence stmts) = catMaybes <$> mapM eval'' stmts

eval'' :: Stmt -> Eval (Maybe Output)
eval'' (AssignmentExpr expr) = Nothing <$ evalAssignmentExpr expr
eval'' (OutputExpr expr)     = Just <$> evalOutputExpr expr

evalAssignmentExpr :: AssignmentExpr -> Eval ()
evalAssignmentExpr (Assignment var expr) = do
  val <- evalIntegerExpr expr
  modify (insert var val)

evalOutputExpr :: OutputExpr -> Eval Output
evalOutputExpr (Output expr name) = do
  val <- evalIntegerExpr expr
  return (val, name)

evalIntegerExpr :: IntegerExpr -> Eval Integer
evalIntegerExpr expr = case expr of
  Constant n     -> return n
  Variable v -> do
    env <- get
    case lookup v env of
      Just n -> return n
      Nothing -> error $ "variable '" ++ show v ++ "' not defined"
  IntegerNegation e    -> negate   <$> evalIntegerExpr e
  Sum e1 e2            -> (+)      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Subtraction e1 e2    -> (-)      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Product e1 e2        -> (*)      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Division e1 e2       -> div      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Exponentiation e1 e2 -> (^)      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Equal e1 e2          -> equal    <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  NotEqual e1 e2       -> notEqual <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Smaller e1 e2        -> smaller  <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  Greater e1 e2        -> greater  <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  AtLeast e1 e2        -> atLeast  <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  AtMost e1 e2         -> atMost   <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  LogicalNegation e    -> not'     <$> evalIntegerExpr e
  LogicalAnd e1 e2     -> and'     <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
  LogicalOr e1 e2      -> or'      <$> evalIntegerExpr e1 <*> evalIntegerExpr e2
