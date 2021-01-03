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

integerLiteral :: Parser Integer
integerLiteral = lexeme Lex.decimal

rangeLiteral :: Parser Range
rangeLiteral = Range <$> valueExpr <* symbol ".." <*> valueExpr

repeatLiteral :: Parser Repeat
repeatLiteral = try (RepeatRange <$> rangeLiteral <* symbol ":" <*> valueExpr)
              <|> RepeatValue <$> valueExpr <* symbol ":" <*> valueExpr

sequenceElementLiteral :: Parser SequenceElement
sequenceElementLiteral = try (SequenceRepeat <$> repeatLiteral)
                       <|> try (SequenceRange <$> rangeLiteral)
                       <|> SequenceValue <$> valueExpr

sequenceLiteral :: Parser Sequence
sequenceLiteral = lexeme $ char '{' *> sequenceElementLiteral `sepBy` comma <* char '}'
  where comma = space *> char ',' *> space

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
    xs -> Stmts xs

stmt' :: Parser Stmt
stmt' = AssignmentExpr <$> assignmentExpr
      <|> LoopExpr <$> loopExpr
      <|> OutputExpr <$> outputExpr

-- Assignment Expressions

assignmentExpr :: Parser AssignmentExpr
assignmentExpr = Assignment <$> variable <* symbol ":" <*> valueExpr

-- Loop Expressions

loopExpr :: Parser LoopExpr
loopExpr = Loop <$ symbol "loop" <*> variable <* symbol "over" <*> valueExpr <* symbol "{" <*> stmt <* symbol "}"

-- Output Expressions

outputExpr :: Parser OutputExpr
outputExpr = Output <$ symbol "output" <*> valueExpr <*> optional (symbol "named" *> stringLiteral)

-- Value Expressions

valueExpr :: Parser ValueExpr
valueExpr = makeExprParser valueTerm valueOperatorTable

valueTerm :: Parser ValueExpr
valueTerm = parenthesised valueExpr
          <|> IntegerLiteral <$> integerLiteral
          <|> SequenceLiteral <$> sequenceLiteral
          <|> Variable <$> variable

valueOperatorTable :: [[Operator Parser ValueExpr]]
valueOperatorTable = [[unaryOp "d" DiceLiteral,
                       unaryOp "+" id,
                       unaryOp "-" Negation,
                       unaryOp "!" Not],
                      [binaryOp "d" DiceCollectionLiteral],
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
