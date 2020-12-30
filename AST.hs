module AST where

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

import Grammar (Stmt)
import Parser (stmt)

buildAST :: (String, String) -> Either (ParseErrorBundle Text Void) Stmt
buildAST (scriptName, scriptContent) = runParser stmt scriptName (fromString scriptContent)

debugAST :: (ParseErrorBundle Text Void) -> String
debugAST errorBundle = errorBundlePretty errorBundle
