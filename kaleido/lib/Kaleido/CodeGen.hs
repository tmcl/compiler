{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- We need these to write a ConvertibleStrings instance for
-- ShortByteString
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kaleido.CodeGen (codegenStatement, codegen, steppableCodegen, Env(..), State(..), newEmptyState) where

import Control.Monad
import Control.Monad.State qualified
import Control.Monad.State.Strict qualified
import Data.Map qualified
import Data.String qualified
import Data.Text qualified
import Debug.Trace
import Kaleido.AST qualified
import LLVM.AST qualified
import LLVM.AST.FloatingPointPredicate qualified
import LLVM.AST.Name qualified
import LLVM.AST.Type qualified
import LLVM.IRBuilder.Constant qualified
import LLVM.IRBuilder.Instruction qualified
import LLVM.IRBuilder.Module qualified
import LLVM.IRBuilder.Monad qualified
import LLVM.IRBuilder.Internal.SnocList qualified
import LLVM.Prelude qualified
import LLVM.IRBuilder qualified

data Env = Env
  { operands :: Data.Map.Map Data.Text.Text LLVM.AST.Operand
  , moduleState :: LLVM.IRBuilder.IRBuilderState
  }

registerOperand ::
  (Control.Monad.State.MonadState Env m) =>
  Data.Text.Text ->
  LLVM.AST.Operand ->
  m ()
registerOperand name op =
  Control.Monad.State.modify $ \env ->
    env
      { operands = Data.Map.insert name op (operands env)
      }

type LLVM =
  LLVM.IRBuilder.Module.ModuleBuilderT
    (Control.Monad.State.State Env)

type Codegen = LLVM.IRBuilder.Monad.IRBuilderT LLVM

cs :: Data.Text.Text -> LLVM.Prelude.ShortByteString
cs = Data.String.fromString . Data.Text.unpack

(!) :: (Show b, Show k, Ord k) => Data.Map.Map k b -> k -> b
a ! n = res
  where
    res = case n `Data.Map.lookup` a of
      Just it -> it
      Nothing -> error ("could not find " <> show n <> " in " <> show a)

codegenExpr :: Kaleido.AST.ExprAST -> Codegen LLVM.AST.Operand
codegenExpr (Kaleido.AST.ExprAstNumber (Kaleido.AST.NumberExprAST num)) = pure $ LLVM.IRBuilder.Constant.double num
codegenExpr (Kaleido.AST.ExprAstVariable (Kaleido.AST.VariableExprAst name)) = Control.Monad.State.gets ((! name) . operands)
codegenExpr (Kaleido.AST.ExprAstBinary (Kaleido.AST.BinaryExprAST op lhs rhs)) = do
  lhs' <- codegenExpr lhs
  rhs' <- codegenExpr rhs
  case op of
    Kaleido.AST.Plus -> LLVM.IRBuilder.Instruction.fadd lhs' rhs'
    Kaleido.AST.Minus -> LLVM.IRBuilder.Instruction.fsub lhs' rhs'
    Kaleido.AST.Times -> LLVM.IRBuilder.Instruction.fmul lhs' rhs'
    Kaleido.AST.Divide -> LLVM.IRBuilder.Instruction.fdiv lhs' rhs'
    Kaleido.AST.LT -> do
      comparison <- LLVM.IRBuilder.Instruction.fcmp LLVM.AST.FloatingPointPredicate.ULT lhs' rhs'
      LLVM.IRBuilder.Instruction.uitofp comparison LLVM.AST.Type.double
codegenExpr (Kaleido.AST.ExprAstCall (Kaleido.AST.CallExprAST callee args)) = do
  args' <- mapM (fmap (,[]) . codegenExpr) args
  f <- Control.Monad.State.gets ((! callee) . operands)
  LLVM.IRBuilder.Instruction.call f args'

codegenStatement :: Kaleido.AST.StatementAST -> LLVM LLVM.AST.Operand
codegenStatement (Kaleido.AST.StatementAstExpr body) = do
    nm <- LLVM.IRBuilder.Monad.freshUnName
    codegenBody nm [] body
codegenStatement (Kaleido.AST.StatementAstExtern (Kaleido.AST.PrototypeExprAST name args)) = do
  function <- LLVM.IRBuilder.Module.extern (LLVM.AST.Name.mkName (Data.Text.unpack name)) (LLVM.AST.Type.double <$ args) LLVM.AST.Type.double
  registerOperand name function
  pure function
codegenStatement (Kaleido.AST.StatementAstFunction (Kaleido.AST.FunctionExprAST (Kaleido.AST.PrototypeExprAST name args) body)) = mdo
  registerOperand name function
  function <- codegenBody llvmName args body
  pure function
  where
    llvmName = LLVM.AST.Name.mkName (Data.Text.unpack name)

codegenBody :: LLVM.AST.Name.Name -> [Data.Text.Text] -> Kaleido.AST.ExprAST -> LLVM LLVM.AST.Operand
codegenBody llvmName args body = 
  locally $ do
    params <- mapM mkParam args
    LLVM.IRBuilder.Module.function llvmName params LLVM.AST.Type.double genBody
  where
    mkParam arg = pure (LLVM.AST.Type.double, LLVM.IRBuilder.Module.ParameterName (cs arg))
    genBody :: [LLVM.AST.Operand] -> Codegen ()
    genBody ops = do
      _entry <- LLVM.IRBuilder.Monad.block `LLVM.IRBuilder.Monad.named` "entry"
      forM_ (zip ops args) $ \(op, arg) -> do
        -- addr <- LLVM.IRBuilder.Instruction.alloca (LLVM.AST.Typed.typeOf op) Nothing 0
        -- LLVM.IRBuilder.Instruction.store addr 0 op
        -- registerOperand arg addr
        registerOperand arg op
      Debug.Trace.traceShowM =<< Control.Monad.State.gets operands
      LLVM.IRBuilder.Instruction.ret =<< codegenExpr body

locally :: (Control.Monad.State.MonadState s m) => m a -> m a
locally computation = do
  oldState <- Control.Monad.State.get
  result <- computation
  result <$ Control.Monad.State.put oldState

codegen :: Kaleido.AST.StatementAST -> (LLVM.AST.Operand, [LLVM.AST.Definition])
codegen x = snd $ steppableCodegen newEmptyState x

steppableCodegen :: State -> Kaleido.AST.StatementAST -> (State, (LLVM.AST.Operand, [LLVM.AST.Definition]))
steppableCodegen startState ast = (endState, (operand, definitions))
  where
    endState = State modul env
    definitions = LLVM.IRBuilder.Internal.SnocList.getSnocList . LLVM.IRBuilder.Module.builderDefs $ modul
    ((operand, modul), env) =
      flip Control.Monad.State.runState (cgState startState) $
        runModuleBuilderT (mbsState startState) $
          codegenStatement ast

runModuleBuilderT ::
  (Monad m) =>
  LLVM.IRBuilder.Module.ModuleBuilderState ->
  LLVM.IRBuilder.Module.ModuleBuilderT m a ->
  m (a, LLVM.IRBuilder.Module.ModuleBuilderState)
runModuleBuilderT s (LLVM.IRBuilder.Module.ModuleBuilderT m) =
  Control.Monad.State.runStateT m s

data State = State { mbsState :: LLVM.IRBuilder.Module.ModuleBuilderState, cgState :: Kaleido.CodeGen.Env }

newEmptyState :: State
newEmptyState = State LLVM.IRBuilder.Module.emptyModuleBuilder (Env mempty LLVM.IRBuilder.emptyIRBuilder)

instance (LLVM.IRBuilder.MonadIRBuilder LLVM) where
    liftIRState ma = do
        s <- Control.Monad.State.gets moduleState
        let (a, s') = Control.Monad.State.Strict.runState ma s
        Control.Monad.State.modify (\olds -> olds { moduleState = s' })
        pure a