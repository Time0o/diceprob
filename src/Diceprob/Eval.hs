{-# OPTIONS_GHC -Wall #-}

module Diceprob.Eval (
  Eval,
  eval,
  evalStmt,
  evalAssignmentExpr,
  evalOutputExpr,
  evalValueExpr
) where

import Prelude hiding (lookup)

import Control.Monad.Trans.State.Lazy (State, modify, get, runState)

import Data.List (foldl1')
import Data.Text (Text)
import Data.HashMap.Strict (HashMap, empty, insert, lookup)

import Diceprob.Dice (dn, mdn)
import Diceprob.Error
import Diceprob.Grammar
import Diceprob.Op
import Diceprob.Output (Output)
import Diceprob.Value

type Eval = State (HashMap Text Value)

eval :: (a -> Eval b) -> (a -> b)
eval eval' what = fst $ runState (eval' what) empty

evalStmt :: Stmt -> Eval [Output]
evalStmt expr = case expr of
  Stmts stmts          -> concat   <$> mapM evalStmt stmts
  AssignmentExpr expr' -> const [] <$> evalAssignmentExpr expr'
  LoopExpr expr'       -> concat   <$> evalLoopExpr expr'
  BranchExpr expr'     -> id       <$> evalBranchExpr expr'
  OutputExpr expr'     -> (:[])    <$> evalOutputExpr expr'

evalAssignmentExpr :: AssignmentExpr -> Eval ()
evalAssignmentExpr (Assignment var expr) = do
  val <- evalValueExpr expr
  modify $ insert var val

evalLoopExpr :: LoopExpr -> Eval [[Output]]
evalLoopExpr (Loop var over stmt) = do
  over' <- evalValueExpr over
  case over' of
    Sequence over'' -> mapM exec over''
    v               -> valueError err_loop_var v
    where assign x = modify $ insert var (Integer x)
          exec   x = assign x >> evalStmt stmt

evalBranchExpr :: BranchExpr -> Eval [Output]
evalBranchExpr expr = case expr of
  Branch cond ifStmt maybeElseStmt -> do
   cond' <- evalValueExpr cond
   case cond' of
     Integer i -> if i == 0 then elseStmt' else ifStmt'
     v         -> valueError err_cond_var v
     where ifStmt'   = evalStmt ifStmt
           elseStmt' = case maybeElseStmt of
              Nothing       -> return []
              Just elseStmt -> evalStmt elseStmt

evalOutputExpr :: OutputExpr -> Eval Output
evalOutputExpr out = case out of
  NamedOutput val name -> do
    val'  <- valueToDice  <$> evalValueExpr val
    name' <- foldl1' (<>) <$> mapM tryExpandVariable name
    return (val', Just name')
      where tryExpandVariable s = case s of
              Left s' -> do
                return s'
              Right var -> do
                var' <- evalVariable var
                return $ valueToText var'
  UnnamedOutput val -> do
    val' <- valueToDice <$> evalValueExpr val
    return (val', Nothing)

evalSequenceValue :: ValueExpr -> Eval [Int]
evalSequenceValue v = do
  v' <- evalValueExpr v
  return $ valueToSequence v'

evalSequenceRange :: Range -> Eval [Int]
evalSequenceRange (Range from to) = do
  from' <- evalValueExpr from
  to'   <- evalValueExpr to
  case (from', to') of
    (Integer from'', Integer to'') -> return [from''..to'']
    (_, Integer _)                 -> valueError err_seq_range_to to'
    _                              -> valueError err_seq_range_to from'

evalSequenceRepeat :: Repeat -> Eval [Int]
evalSequenceRepeat r = do
    what <- case r of
              RepeatValue v _  -> evalSequenceValue v
              RepeatRange r' _ -> evalSequenceRange r'
    times <- case r of
              RepeatValue _ t -> evalValueExpr t
              RepeatRange _ t -> evalValueExpr t
    case times of
      Integer times' -> return . concat . replicate times' $ what
      v              -> valueError err_seq_repeat v

evalSequence :: Sequence -> Eval [Int]
evalSequence s = concat <$> mapM expandElement s
  where expandElement e = case e of
          SequenceValue v  -> evalSequenceValue v
          SequenceRange r  -> evalSequenceRange r
          SequenceRepeat r -> evalSequenceRepeat r

evalVariable :: Text -> Eval Value
evalVariable var = do
  env <- get
  case lookup var env of
    Just x  -> return x
    Nothing -> errorf err_var var

evalValueExpr :: ValueExpr -> Eval Value
evalValueExpr expr = case expr of
  IntegerLiteral x  -> return $ Integer x
  SequenceLiteral x -> Sequence <$> evalSequence x
  DiceLiteral e -> do
    v <- evalValueExpr e
    let n = valueToInteger v -- XXX custom dice
    return $ Dice (dn n)
  DiceCollectionLiteral e e' -> do
    v <- evalValueExpr e
    v' <- evalValueExpr e'
    let (m, n) = (valueToInteger v, valueToInteger v') -- XXX custom dice
    return . DiceCollection $ mdn m n
  Variable var -> evalVariable var
  Negation e          -> valueUnaryOp  (#-)          <$> evalValueExpr e
  Sum e e'            -> valueBinaryOp (#+)          <$> evalValueExpr e <*> evalValueExpr e'
  Subtraction e e'    -> valueBinaryOp (#--)         <$> evalValueExpr e <*> evalValueExpr e'
  Product e e'        -> valueBinaryOp (#*)          <$> evalValueExpr e <*> evalValueExpr e'
  Division e e'       -> valueBinaryOp (#/)          <$> evalValueExpr e <*> evalValueExpr e'
  Exponentiation e e' -> valueBinaryOp (#^)          <$> evalValueExpr e <*> evalValueExpr e'
  Equal e e'          -> valueSequenceBinaryOp (#=)  <$> evalValueExpr e <*> evalValueExpr e'
  NotEqual e e'       -> valueSequenceBinaryOp (#!=) <$> evalValueExpr e <*> evalValueExpr e'
  Smaller e e'        -> valueSequenceBinaryOp (#<)  <$> evalValueExpr e <*> evalValueExpr e'
  Greater e e'        -> valueSequenceBinaryOp (#>)  <$> evalValueExpr e <*> evalValueExpr e'
  AtMost e e'         -> valueSequenceBinaryOp (#<=) <$> evalValueExpr e <*> evalValueExpr e'
  AtLeast e e'        -> valueSequenceBinaryOp (#>=) <$> evalValueExpr e <*> evalValueExpr e'
  Not e               -> valueUnaryOp  (#!)          <$> evalValueExpr e
  And e1 e2           -> valueBinaryOp (#&)          <$> evalValueExpr e1 <*> evalValueExpr e2
  Or e1 e2            -> valueBinaryOp (#|)          <$> evalValueExpr e1 <*> evalValueExpr e2
  Length e            -> Integer . valueLength       <$> evalValueExpr e
  ValueAccess e1 e2   -> valueAccess                 <$> evalValueExpr e1 <*> evalValueExpr e2
