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

import Replace.Megaparsec (splitCap)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex

import Diceprob.Grammar

-- Parser

type Parser = Parsec Void Text

-- Basic

spaceOrComment :: Parser ()
spaceOrComment = Lex.space space1 empty (Lex.skipBlockComment "\\" "\\")

symbol :: Text -> Parser Text
symbol = Lex.symbol spaceOrComment

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme spaceOrComment

parenthesised :: Parser a -> Parser a
parenthesised = between (char '(') (char ')')

braced :: Parser a -> Parser a
braced = between (char '{') (char '}')

bracketed :: Parser a -> Parser a
bracketed = between (char '[') (char ']')

commaSeparated :: Parser a -> Parser [a]
commaSeparated = flip sepBy $ space *> char ',' *> space

-- Literals

integerLiteral :: Parser Integer
integerLiteral = lexeme Lex.decimal

rangeLiteral :: Parser Range
rangeLiteral = Range <$> valueExpr <* symbol ".." <*> valueExpr

repeatLiteral :: Parser Repeat
repeatLiteral = try (RepeatRange <$> rangeLiteral <* symbol ":" <*> valueExpr)
              <|> RepeatValue    <$> valueExpr <* symbol ":" <*> valueExpr

sequenceElementLiteral :: Parser SequenceElement
sequenceElementLiteral = try (SequenceRepeat  <$> repeatLiteral)
                       <|> try (SequenceRange <$> rangeLiteral)
                       <|> SequenceValue      <$> valueExpr

sequenceLiteral :: Parser Sequence
sequenceLiteral = lexeme . braced . commaSeparated $ sequenceElementLiteral

stringLiteral' :: Parser Text
stringLiteral' = fromString <$ char '"' <*> manyTill Lex.charLiteral (char '"')

stringLiteral :: Parser [Either Text Text]
stringLiteral = lexeme $ splitCap variableExpansion <$> stringLiteral'

-- Variables

variable' :: Parser Text
variable' = fromString <$> some (upperChar <|> char '_')

variable :: Parser Text
variable = lexeme $ variable'

variableExpansion :: Parser Text
variableExpansion = bracketed variable'

-- Statments

stmt :: Parser Stmt
stmt = do
  stmtSeq <- some stmt'
  return $ case stmtSeq of
    (x:[]) -> x
    xs     -> Stmts xs

stmt' :: Parser Stmt
stmt' = AssignmentExpr <$> assignmentExpr
      <|> LoopExpr     <$> loopExpr
      <|> BranchExpr   <$> branchExpr
      <|> OutputExpr   <$> outputExpr

-- Assignment Expressions

assignmentExpr :: Parser AssignmentExpr
assignmentExpr = Assignment <$> variable <* symbol ":" <*> valueExpr

-- Loop Expressions

loopExpr :: Parser LoopExpr
loopExpr = Loop <$ symbol "loop" <*> variable
                <* symbol "over" <*> valueExpr
                <* symbol "{" <*> stmt <* symbol "}"

-- Branch Expressions

branchExpr :: Parser BranchExpr
branchExpr = Branch <$ symbol "if" <*> valueExpr <* symbol "{" <*> stmt <* symbol "}"
                    <*> optional (symbol "else" *> symbol "{" *> stmt <* symbol "}")

-- Output Expressions

outputExpr :: Parser OutputExpr
outputExpr = try namedOutput
           <|> unnamedOutput
  where namedOutput   = NamedOutput   <$ symbol "output" <*> valueExpr
                                      <* symbol "named" <*> stringLiteral
        unnamedOutput = UnnamedOutput <$ symbol "output" <*> valueExpr

-- Value Expressions

valueExpr :: Parser ValueExpr
valueExpr = makeExprParser valueTerm valueOperatorTable

valueTerm :: Parser ValueExpr
valueTerm = parenthesised valueExpr
          <|> IntegerLiteral  <$> integerLiteral
          <|> SequenceLiteral <$> sequenceLiteral
          <|> Variable        <$> variable

valueOperatorTable :: [[Operator Parser ValueExpr]]
valueOperatorTable = [[unaryOp "d" DiceLiteral,
                       unaryOp "+" id,
                       unaryOp "-" Negation,
                       unaryOp "!" Not,
                       unaryOp "#" Length],
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

unaryOp :: Text -> (a -> a) -> Operator Parser a
unaryOp name f = Prefix (f <$ symbol name)

binaryOp :: Text -> (a -> a -> a) -> Operator Parser a
binaryOp name f = InfixL (f <$ symbol name)
