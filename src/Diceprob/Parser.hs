{-# OPTIONS_GHC -Wall -fno-cse #-}

{-# LANGUAGE OverloadedStrings #-}

module Diceprob.Parser (
  Parser,
  stmt,
  assignmentExpr,
  outputExpr,
  valueExpr
) where

import Control.Monad.Combinators.Expr

import Data.String (fromString)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Diceprob.Dice (Dice, dn)
import Diceprob.Grammar

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

diceLiteral :: Parser Dice
diceLiteral = lexeme $ dn <$ symbol "d" <*> Lex.decimal

diceCollectionLiteral :: Parser [Dice]
diceCollectionLiteral = lexeme $ (\num n -> replicate num (dn n)) <$> Lex.decimal <* symbol "d" <*> Lex.decimal

integerLiteral :: Parser Integer
integerLiteral = lexeme Lex.decimal

integerSequenceLiteral :: Parser [Integer]
integerSequenceLiteral = lexeme $ char '{' *> integerLiteral `sepBy` (space *> char ',' *> space) <* char '}'

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
  stmtSeq <- some stmt'
  return $ case stmtSeq of
    (x:[]) -> x
    xs -> Sequence xs

stmt' :: Parser Stmt
stmt' = AssignmentExpr <$> assignmentExpr
      <|> OutputExpr <$> outputExpr

-- Assignment Expressions

assignmentExpr :: Parser AssignmentExpr
assignmentExpr = Assignment <$> variable <* symbol ":" <*> valueExpr

-- Output Expressions

outputExpr :: Parser OutputExpr
outputExpr = Output <$ symbol "output" <*> valueExpr <*> optional (symbol "named" *> stringLiteral)

-- Value Expressions

valueExpr :: Parser ValueExpr
valueExpr = makeExprParser valueTerm valueOperatorTable

valueTerm :: Parser ValueExpr
valueTerm = parenthesised valueExpr
          <|> Literal <$> literal
          <|> Variable <$> variable
  where literal :: Parser Literal
        literal = try (DiceLiteral <$> diceLiteral)
                <|> try (DiceCollectionLiteral <$> diceCollectionLiteral)
                <|> IntegerLiteral <$> integerLiteral
                <|> IntegerSequenceLiteral <$> integerSequenceLiteral

valueOperatorTable :: [[Operator Parser ValueExpr]]
valueOperatorTable = [[unaryOp "+" id,
                       unaryOp "-" Negation,
                       unaryOp "!" Not],
                      [binaryOp "^" Exponentiation],
                      [binaryOp "*" Product, binaryOp "/" Division],
                      [binaryOp "+" Sum, binaryOp "-" Subtraction],
                      [binaryOp "=" Equal,
                       binaryOp "!=" NotEqual,
                       binaryOp ">=" AtLeast,
                       binaryOp "<=" AtMost,
                       binaryOp "<" Smaller,
                       binaryOp ">" Greater],
                      [binaryOp "&" And, binaryOp "|" Or]]
