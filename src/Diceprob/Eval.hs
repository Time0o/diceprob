{-# OPTIONS_GHC -Wall #-}

module Diceprob.Eval (
  Output,
  Eval,
  eval,
  evalStmt,
  evalAssignmentExpr,
  evalOutputExpr,
  evalValueExpr
) where

import Prelude hiding (lookup)

import Control.Monad.Trans.State.Lazy (State, modify, get, runState)

import Data.Text (Text)
import Data.HashMap.Strict (HashMap, empty, insert, lookup)

import Diceprob.Dice (Dice)
import Diceprob.Grammar
import Diceprob.Op
import Diceprob.Value

type Output = (Dice, Maybe Text)

type Eval = State (HashMap Text Value)

eval :: (a -> Eval b) -> (a -> b)
eval eval' what = fst $ runState (eval' what) empty

evalStmt :: Stmt -> Eval [Output]
evalStmt (Stmts stmts)         = concat   <$> mapM evalStmt stmts
evalStmt (AssignmentExpr expr) = const [] <$> evalAssignmentExpr expr
evalStmt (OutputExpr expr)     = (:[])    <$> evalOutputExpr expr

evalAssignmentExpr :: AssignmentExpr -> Eval ()
evalAssignmentExpr (Assignment var expr) = do
  val <- evalValueExpr expr
  modify (insert var val)

evalOutputExpr :: OutputExpr -> Eval Output
evalOutputExpr (Output expr name) = do
  val <- evalValueExpr $ expr
  return (valueToDice val, name)

evalValueExpr :: ValueExpr -> Eval Value
evalValueExpr expr = case expr of
  Literal l -> case l of
    DiceLiteral x           -> return $ Dice x
    DiceCollectionLiteral x -> return $ DiceCollection x
    IntegerLiteral x        -> return $ Integer x
    SequenceLiteral x       -> do
      expanded <- expandSequence x
      return $ Sequence expanded
        where expandElement :: SequenceElement -> Eval [Integer]
              expandElement e = case e of
                Element e' -> do
                  value <- evalValueExpr e'
                  return $ valueToSequence value
                Range from to -> do
                  from' <- evalValueExpr from
                  to'   <- evalValueExpr to
                  case (from', to') of
                    (Integer from'', Integer to'') -> return [from''..to'']
                    _                              -> error "sequence range must begin and end with integer"
                Repeat what times -> do
                  what'  <- evalValueExpr what
                  times' <- evalValueExpr times
                  case times' of
                    Integer times'' -> return $ concat . replicate times''' $ what''
                      where what''   = valueToSequence what'
                            times''' = fromIntegral times''
                    _               -> error "sequence multiplier must be an integer"
              expandSequence :: [SequenceElement] -> Eval [Integer]
              expandSequence s = concat <$> mapM expandElement s
  Variable v -> do
    env <- get
    case lookup v env of
      Just x -> return x
      Nothing -> error $ "variable '" ++ show v ++ "' not defined"
  Negation e          -> valueUnaryOp  (#-)  <$> evalValueExpr e
  Sum e e'            -> valueBinaryOp (#+)  <$> evalValueExpr e <*> evalValueExpr e'
  Subtraction e e'    -> valueBinaryOp (#--) <$> evalValueExpr e <*> evalValueExpr e'
  Product e e'        -> valueBinaryOp (#*)  <$> evalValueExpr e <*> evalValueExpr e'
  Division e e'       -> valueBinaryOp (#/)  <$> evalValueExpr e <*> evalValueExpr e'
  Exponentiation e e' -> valueBinaryOp (#^)  <$> evalValueExpr e <*> evalValueExpr e'
  Equal e e'          -> valueBinaryOp (#=)  <$> evalValueExpr e <*> evalValueExpr e'
  NotEqual e e'       -> valueBinaryOp (#!=) <$> evalValueExpr e <*> evalValueExpr e'
  Smaller e e'        -> valueBinaryOp (#<)  <$> evalValueExpr e <*> evalValueExpr e'
  Greater e e'        -> valueBinaryOp (#>)  <$> evalValueExpr e <*> evalValueExpr e'
  AtMost e e'         -> valueBinaryOp (#<=) <$> evalValueExpr e <*> evalValueExpr e'
  AtLeast e e'        -> valueBinaryOp (#>=) <$> evalValueExpr e <*> evalValueExpr e'
  Not e               -> valueUnaryOp  (#!)  <$> evalValueExpr e
  And e1 e2           -> valueBinaryOp (#&)  <$> evalValueExpr e1 <*> evalValueExpr e2
  Or e1 e2            -> valueBinaryOp (#|)  <$> evalValueExpr e1 <*> evalValueExpr e2
