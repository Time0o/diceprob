module AST where

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

import Eval (eval)
import Grammar (Stmt)
import Parser (stmt)

type AST = Either (ParseErrorBundle Text Void) Stmt

buildAST :: (String, String) -> AST
buildAST (scriptName, scriptContent) = runParser stmt scriptName (fromString scriptContent)

evalAST :: AST -> String
evalAST ast = either errorBundlePretty show ast -- XXX do something with evaluation
