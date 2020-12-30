{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-cse #-}

module Parser where

import Control.Monad.Combinators.Expr

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Grammar

-- Parser

type Parser = Parsec Void Text

-- Tokens

spaceOrComment :: Parser ()
spaceOrComment = Lex.space space1 empty (Lex.skipBlockComment "\\" "\\")

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceOrComment

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceOrComment

-- Basic

parenthesised :: Parser a -> Parser a
parenthesised = between (symbol "(") (symbol ")")

constant :: Parser Integer
constant = lexeme Lex.decimal

variable :: Parser Text
variable = lexeme $ fromString <$> some (upperChar <|> char '_')

-- Statments

stmt :: Parser Stmt
stmt = do
  seq <- some stmt'
  return $ case seq of
    (x:[]) -> x
    xs -> Sequence xs

stmt' :: Parser Stmt
stmt' = AssignmentExpr <$> assignmentExpr

-- Arithmetic Expressions

arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = makeExprParser arithmeticTerm operatorTable

arithmeticTerm :: Parser ArithmeticExpr
arithmeticTerm = (parenthesised arithmeticExpr) <|> Constant <$> constant <|> Variable <$> variable

unaryOp :: Text
        -> (ArithmeticExpr -> ArithmeticExpr)
        -> Operator Parser ArithmeticExpr
unaryOp name f = Prefix (f <$ symbol name)

binaryOp :: Text
         -> (ArithmeticExpr -> ArithmeticExpr -> ArithmeticExpr)
         -> Operator Parser ArithmeticExpr
binaryOp name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser ArithmeticExpr]]
operatorTable = [[unaryOp "+" id, unaryOp "-" Negation],
                 [binaryOp "^" Exponentiation],
                 [binaryOp "*" Product, binaryOp "/" Division],
                 [binaryOp "+" Sum, binaryOp "-" Subtraction]]

-- Assignment

assignmentExpr :: Parser AssignmentExpr
assignmentExpr = Assignment <$> variable <* symbol ":" <*> arithmeticExpr
