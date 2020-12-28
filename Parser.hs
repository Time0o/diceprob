{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-cse #-}

module Parser where

import Control.Monad.Combinators.Expr

import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Grammar

type Parser = Parsec Void Text

spaceOrComment :: Parser ()
spaceOrComment = Lex.space space1 empty (Lex.skipBlockComment "\\" "\\")

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceOrComment

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceOrComment

expr :: Parser Expr
expr = makeExprParser term operatorTable

term :: Parser Expr
term = choice [ parens expr, integer ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Expr
integer = Integer <$> lexeme Lex.decimal

unaryOp :: Text
        -> (Expr -> Expr)
        -> Operator Parser Expr
unaryOp name f = Prefix (f <$ symbol name)

binaryOp :: Text
         -> (Expr -> Expr -> Expr)
         -> Operator Parser Expr
binaryOp name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable = [[unaryOp "+" id, unaryOp "-" Negation],
                 [binaryOp "^" Exponentiation],
                 [binaryOp "*" Product, binaryOp "/" Division],
                 [binaryOp "+" Sum, binaryOp "-" Subtraction]]
