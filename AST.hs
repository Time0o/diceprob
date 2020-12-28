module AST where

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

import Eval (eval)
import Grammar (Expr)
import Parser (expr)

type AST = Either (ParseErrorBundle Text Void) Expr

buildAST :: (String, String) -> AST
buildAST (scriptName, scriptContent) = runParser expr scriptName (fromString scriptContent)

evalAST :: AST -> String
evalAST ast = either errorBundlePretty (show . eval) ast
