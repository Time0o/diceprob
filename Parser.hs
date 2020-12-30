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

integerLiteral :: Parser Integer
integerLiteral = lexeme Lex.decimal

stringLiteral :: Parser Text
stringLiteral = lexeme $ fromString <$ char '"' <*> manyTill Lex.charLiteral (char '"')

variable :: Parser Text
variable = lexeme $ fromString <$> some (upperChar <|> char '_')

parenthesised :: Parser a -> Parser a
parenthesised = between (symbol "(") (symbol ")")

unaryOp :: Text
        -> (a -> a)
        -> Operator Parser a
unaryOp name f = Prefix (f <$ symbol name)

binaryOp :: Text
         -> (a -> a -> a)
         -> Operator Parser a
binaryOp name f = InfixL (f <$ symbol name)

-- Statments

stmt :: Parser Stmt
stmt = do
  seq <- some stmt'
  return $ case seq of
    (x:[]) -> x
    xs -> Sequence xs

stmt' :: Parser Stmt
stmt' = AssignmentExpr <$> assignmentExpr
      <|> OutputExpr <$> outputExpr

-- Integer Expressions

integerExpr :: Parser IntegerExpr
integerExpr = makeExprParser integerTerm integerOperatorTable

integerTerm :: Parser IntegerExpr
integerTerm = (parenthesised integerExpr)
            <|> Constant <$> integerLiteral
            <|> Variable <$> variable

integerOperatorTable :: [[Operator Parser IntegerExpr]]
integerOperatorTable = [[unaryOp "+" id,
                         unaryOp "-" IntegerNegation,
                         unaryOp "!" LogicalNegation],
                        [binaryOp "^" Exponentiation],
                        [binaryOp "*" Product, binaryOp "/" Division],
                        [binaryOp "+" Sum, binaryOp "-" Subtraction],
                        [binaryOp "=" Equal,
                          binaryOp "!=" NotEqual,
                          binaryOp "<" Smaller,
                          binaryOp ">" Greater,
                          binaryOp ">=" AtLeast,
                          binaryOp "<=" AtMost],
                        [binaryOp "&" LogicalAnd, binaryOp "|" LogicalOr]]

-- Assignment

assignmentExpr :: Parser AssignmentExpr
assignmentExpr = Assignment <$> variable <* symbol ":" <*> integerExpr

-- Output

outputExpr :: Parser OutputExpr
outputExpr = Output <$ symbol "output" <*> integerExpr <*> optional (symbol "named" *> stringLiteral)
