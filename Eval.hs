module Eval where

import Prelude hiding (lookup)

import Control.Monad.Trans.State.Lazy (State, modify, get, runState)

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, empty, insert, lookup)

import Dice
import Grammar

type Output = (Dice, Maybe Text)

type Eval = State (HashMap Text Dice)

eval :: Stmt -> [Output]
eval stmt = fst $ runState (eval' stmt) empty

eval' :: Stmt -> Eval [Output]
eval' (Sequence stmts) = catMaybes <$> mapM eval'' stmts

eval'' :: Stmt -> Eval (Maybe Output)
eval'' (AssignmentExpr expr) = Nothing <$ evalAssignmentExpr expr
eval'' (OutputExpr expr)     = Just <$> evalOutputExpr expr

evalAssignmentExpr :: AssignmentExpr -> Eval ()
evalAssignmentExpr (Assignment var expr) = do
  val <- evalDiceExpr expr
  modify (insert var val)

evalOutputExpr :: OutputExpr -> Eval Output
evalOutputExpr (Output expr name) = do
  val <- evalDiceExpr expr
  return (val, name)

evalDiceExpr :: DiceExpr -> Eval Dice
evalDiceExpr expr = case expr of
  DiceLiteral d     -> return d
  IntegerLiteral n  -> return $ fromInteger n
  Variable v        -> do
    env <- get
    case lookup v env of
      Just d -> return d
      Nothing -> error $ "variable '" ++ show v ++ "' not defined"
  Negation e           -> negate       <$> evalDiceExpr e
  Sum e1 e2            -> (+)          <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Subtraction e1 e2    -> (-)          <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Product e1 e2        -> (*)          <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Division e1 e2       -> (/)          <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Exponentiation e1 e2 -> dicePower    <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Equal e1 e2          -> diceEqual    <$> evalDiceExpr e1 <*> evalDiceExpr e2
  NotEqual e1 e2       -> diceNotEqual <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Smaller e1 e2        -> diceSmaller  <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Greater e1 e2        -> diceGreater  <$> evalDiceExpr e1 <*> evalDiceExpr e2
  AtLeast e1 e2        -> diceAtLeast  <$> evalDiceExpr e1 <*> evalDiceExpr e2
  AtMost e1 e2         -> diceAtMost   <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Not e                -> diceNot      <$> evalDiceExpr e
  And e1 e2            -> diceAnd      <$> evalDiceExpr e1 <*> evalDiceExpr e2
  Or e1 e2             -> diceOr       <$> evalDiceExpr e1 <*> evalDiceExpr e2
