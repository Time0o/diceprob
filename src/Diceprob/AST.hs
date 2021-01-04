{-# OPTIONS_GHC -Wall #-}

module Diceprob.AST (AST, buildAST, debugAST) where

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

import Diceprob.Grammar (Stmt)
import Diceprob.Parser (stmt)

type AST a = Either (ParseErrorBundle Text Void) a

buildAST :: (String, String) -> AST Stmt
buildAST (scriptName, scriptContent) = runParser stmt scriptName (fromString scriptContent)

debugAST :: (ParseErrorBundle Text Void) -> String
debugAST errorBundle = errorBundlePretty errorBundle
