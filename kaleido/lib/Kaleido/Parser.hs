{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Kaleido.Parser where

import Control.Applicative
import Control.Monad
import Data.Char qualified
import Data.Text qualified
import Data.Void qualified
import Kaleido.AST qualified
import Text.Megaparsec qualified
import Text.Megaparsec.Char qualified
import Text.Megaparsec.Char.Lexer qualified
import Control.Monad.Combinators.Expr qualified

type Parser = Text.Megaparsec.Parsec Data.Void.Void Data.Text.Text

type ParserErrorBundle = Text.Megaparsec.ParseErrorBundle Data.Text.Text Data.Void.Void

sc :: Parser ()
sc =
  Text.Megaparsec.Char.Lexer.space
    Text.Megaparsec.Char.space1
    (Text.Megaparsec.Char.Lexer.skipLineComment "#")
    mzero

lexeme :: Parser a -> Parser a
lexeme = Text.Megaparsec.Char.Lexer.lexeme sc

number :: Parser Kaleido.AST.ExprAST
number =
  Kaleido.AST.ExprAstNumber . Kaleido.AST.NumberExprAST
    <$> Text.Megaparsec.Char.Lexer.signed mzero (Text.Megaparsec.try Text.Megaparsec.Char.Lexer.float <|> Text.Megaparsec.Char.Lexer.decimal)


symbol :: Data.Text.Text -> Parser Data.Text.Text
symbol = Text.Megaparsec.Char.Lexer.symbol sc

parens :: Parser a -> Parser a
parens = Text.Megaparsec.between (symbol "(") (symbol ")")

-- parseParenExpr = parens parseExpression

expr :: Parser Kaleido.AST.ExprAST
expr = Control.Monad.Combinators.Expr.makeExprParser
  term
   [[binary "*" (mkBin '*')
    , binary "/" (mkBin '/')]
   ,[binary "+" (mkBin '+')
    , binary "-" (mkBin '-')]
   ]

   where mkBin op lhs rhs = Kaleido.AST.ExprAstBinary Kaleido.AST.BinaryExprAST { .. }


binary :: Data.Text.Text -> (a -> a -> a) -> Control.Monad.Combinators.Expr.Operator Parser a
binary name f = Control.Monad.Combinators.Expr.InfixL (f <$ symbol name)

term :: Parser Kaleido.AST.ExprAST
term = Text.Megaparsec.try functionCall <|> number <|> variable

functionCall :: Parser Kaleido.AST.ExprAST
functionCall = do
    callee <- identifier
    args <- parens $ term `Text.Megaparsec.sepBy` symbol ","
    pure $ Kaleido.AST.ExprAstCall $ Kaleido.AST.CallExprAST { .. }


identifier :: Parser Data.Text.Text
identifier = lexeme do
  letter <- Text.Megaparsec.Char.letterChar
  rest <- Text.Megaparsec.takeWhileP (Just "identifier") Data.Char.isAlphaNum
  pure $ Data.Text.cons letter rest

variable :: Parser Kaleido.AST.ExprAST
variable =
  Kaleido.AST.ExprAstVariable . Kaleido.AST.VariableExprAst <$> identifier

proto :: Parser Kaleido.AST.PrototypeExprAST
proto = do
  name <- identifier
  args <- parens $ many identifier
  pure $ Kaleido.AST.PrototypeExprAST {..}

function :: Parser Kaleido.AST.ExprAST
function = do
  _ <- lexeme "def"
  p <- proto
  f <- expr
  pure $ Kaleido.AST.ExprAstFunction $ Kaleido.AST.FunctionExprAST p f

extern :: Parser Kaleido.AST.ExprAST
extern = do
  _ <- lexeme "extern"
  Kaleido.AST.ExprAstPrototype <$> proto

defn :: Parser Kaleido.AST.ExprAST
defn = extern <|> function <|> expr

toplevel :: Parser [Kaleido.AST.ExprAST]
toplevel = Text.Megaparsec.many (defn <* lexeme ";")

parseTopLevel :: Data.Text.Text -> Either ParserErrorBundle [Kaleido.AST.ExprAST]
parseTopLevel = Text.Megaparsec.parse (toplevel <* Text.Megaparsec.eof) "<stdin>"