{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Kaleido.AST where

import Data.Text qualified
import Prelude (Double)
import Text.Show qualified
import Data.Eq qualified

data StatementAST
  = StatementAstExpr ExprAST
  | StatementAstExtern PrototypeExprAST
  | StatementAstFunction FunctionExprAST
  deriving (Text.Show.Show, Data.Eq.Eq)

data ExprAST
  = ExprAstNumber NumberExprAST
  | ExprAstVariable VariableExprAST
  | ExprAstBinary BinaryExprAST
  | ExprAstCall CallExprAST
  deriving (Text.Show.Show, Data.Eq.Eq)

newtype NumberExprAST = NumberExprAST
  { val :: Double
  }
  deriving (Text.Show.Show, Data.Eq.Eq)

newtype VariableExprAST = VariableExprAst
  { name :: Data.Text.Text
  }
  deriving (Text.Show.Show, Data.Eq.Eq)

data BinOp = Plus | Times | Divide | Minus | LT
  deriving (Text.Show.Show, Data.Eq.Eq)

data BinaryExprAST = BinaryExprAST
  { op :: BinOp,
    lhs :: ExprAST,
    rhs :: ExprAST
  }
  deriving (Text.Show.Show, Data.Eq.Eq)

data CallExprAST = CallExprAST
  { callee :: Data.Text.Text,
    args :: [ExprAST]
  }
  deriving (Text.Show.Show, Data.Eq.Eq)

data PrototypeExprAST = PrototypeExprAST
  { name :: Data.Text.Text,
    args :: [Data.Text.Text]
  }
  deriving (Text.Show.Show, Data.Eq.Eq)

data FunctionExprAST = FunctionExprAST
  { prototype :: PrototypeExprAST,
    body :: ExprAST
  }
  deriving (Text.Show.Show, Data.Eq.Eq)