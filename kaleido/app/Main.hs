{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Main (main) where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.ByteString qualified
import Data.Foldable
import Data.Function
import Data.Text qualified
import Data.Text.Lazy.IO qualified
import Kaleido.AST qualified
import Kaleido.CodeGen qualified
import Kaleido.Parser qualified
import LLVM.AST qualified
import LLVM.Analysis qualified
import LLVM.Context qualified
import LLVM.ExecutionEngine qualified
import LLVM.IRBuilder.Internal.SnocList qualified
import LLVM.IRBuilder.Module qualified
import LLVM.Module qualified
import LLVM.PassManager qualified
import LLVM.Pretty qualified
import LLVM.Transforms qualified
import System.Console.Haskeline qualified
import Text.Megaparsec qualified
import Foreign.Ptr qualified
import LLVM.Internal.ExecutionEngine qualified
import Data.IORef qualified
import Debug.Trace qualified

import LLVM.Internal.DecodeAST qualified

import Control.Monad

import qualified LLVM.Internal.FFI.PtrHierarchy 
import qualified LLVM.Internal.FFI.ExecutionEngine 
import qualified LLVM.Internal.FFI.Module 

import LLVM.Internal.Module qualified
import LLVM.Internal.Context qualified

import Control.Monad.Trans.State.Strict


newtype BigEvilErrorType
  = ParseError Kaleido.Parser.ParserErrorBundle

type KaleidoIO m = ExceptT BigEvilErrorType (StateT Kaleido.CodeGen.State m)

process :: MonadIO m => Data.Text.Text -> KaleidoIO m ()
process line = do
    res <-
      Kaleido.Parser.parseTopLevel line
        & first ParseError
        & except
    forM_ res jitKaleidoStatement
  where
    jitKaleidoStatement ::
      MonadIO m => Kaleido.AST.StatementAST ->
      KaleidoIO m ()
    jitKaleidoStatement val = do
      startState <- lift get
      let (endState, (op, defs)) = Kaleido.CodeGen.steppableCodegen startState val
      case val of
        Kaleido.AST.StatementAstExpr {} -> pure ()
        _ -> lift $ put endState

      liftIO do 
        Data.Text.Lazy.IO.putStrLn $ LLVM.Pretty.ppll op
        forM_ (last (Just <$> defs)) \def -> do
          Data.Text.Lazy.IO.putStrLn . LLVM.Pretty.ppll $ def
          llvmModuleFromDefs defs `jitDefinitionInModule` def

handleErrors :: MonadIO m => 
      Either
        BigEvilErrorType
        a ->
      m ()
handleErrors = \case
      Right _ -> pure ()
      Left (ParseError e) -> liftIO $ putStrLn (Text.Megaparsec.errorBundlePretty e)

main :: IO ()
main = 
  void $ System.Console.Haskeline.runInputT
    System.Console.Haskeline.defaultSettings (runStateT loop Kaleido.CodeGen.newEmptyState)
     
  where
    loop :: StateT Kaleido.CodeGen.State (System.Console.Haskeline.InputT IO) ()
    loop = do
      minput <- lift $ System.Console.Haskeline.getInputLine "ready> "
      case minput of
        Nothing -> do
          endState <- get
          let definitions = LLVM.IRBuilder.Internal.SnocList.getSnocList . LLVM.IRBuilder.Module.builderDefs $ Kaleido.CodeGen.mbsState endState
              modul = llvmModuleFromDefs definitions
           in liftIO $ finalizeModule modul
        Just input -> runExceptT (process (Data.Text.pack input)) >>= handleErrors >> loop

llvmModuleFromDefs :: [LLVM.AST.Definition] -> LLVM.AST.Module
llvmModuleFromDefs definitions =
  LLVM.AST.defaultModule {LLVM.AST.moduleName = "my kaleido jit", LLVM.AST.moduleDefinitions = definitions}

finalizeModule :: LLVM.AST.Module -> IO ()
finalizeModule llvmModule = withSatisfactoryModule llvmModule \modl -> do
  LLVM.Module.moduleLLVMAssembly modl >>= Data.ByteString.putStr

jitDefinitionInModule :: LLVM.AST.Module -> LLVM.AST.Definition -> IO ()
jitDefinitionInModule llvmModule def = LLVM.Context.withContext \ctx -> LLVM.Module.withModuleFromAST ctx llvmModule \modl -> do
  verifyAndOptimizeModule modl

  ast <- LLVM.Module.moduleAST modl
  let fixedDefs = LLVM.AST.moduleDefinitions ast
  mapM_ (Data.Text.Lazy.IO.putStrLn . LLVM.Pretty.ppll) (fixedDefs `findCorrespondingDefinition` def)

  let mname = getName def
  case mname of
    Nothing -> putStrLn "couldn't find a name/unname for the current expression"
    Just it@LLVM.AST.UnName {} -> execute ctx modl it
    Just _it@LLVM.AST.Name {} -> putStrLn "not executing named function; call it yourself"

  where 
    execute :: LLVM.Internal.Context.Context -> LLVM.Internal.Module.Module -> LLVM.AST.Name -> IO ()
    execute ctx modl name = 
      withJit ctx \executionEngine -> 
        LLVM.ExecutionEngine.withModuleInEngine executionEngine modl \moduleInEE -> do 
          mfunbody <- getFunctionFromMCJIT name moduleInEE 
          forM_ mfunbody \funbody -> do
            res <- runFunPtr (Foreign.Ptr.castFunPtr @() @(IO Double) funbody)
            putStrLn $ "Evaluated to " <> show res


getName :: LLVM.AST.Definition -> Maybe LLVM.AST.Name
getName (LLVM.AST.GlobalDefinition (LLVM.AST.Function _ _ _ _ _ _ nm _ _ _ _ _ _ _ _ _ _)) = Just nm
getName _ = Nothing


findCorrespondingDefinition :: [LLVM.AST.Definition] -> LLVM.AST.Definition -> Maybe LLVM.AST.Definition
findCorrespondingDefinition [] _ = Nothing
findCorrespondingDefinition ((f@(LLVM.AST.GlobalDefinition (LLVM.AST.Function _ _ _ _ _ _ nm1 _ _ _ _ _ _ _ _ _ _)) : _)) (LLVM.AST.GlobalDefinition (LLVM.AST.Function _ _ _ _ _ _ nm2 _ _ _ _ _ _ _ _ _ _))
  | (nm1 :: LLVM.AST.Name) == nm2 = Just f
findCorrespondingDefinition (_ : rst) def = findCorrespondingDefinition rst def

withSatisfactoryModule :: LLVM.AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withSatisfactoryModule llvmModule ma = LLVM.Context.withContext \ctx -> LLVM.Module.withModuleFromAST ctx llvmModule \modl -> do
  verifyAndOptimizeModule modl
  ma modl

withJit :: LLVM.Context.Context -> (LLVM.ExecutionEngine.MCJIT -> IO a) -> IO a
withJit ctx = LLVM.ExecutionEngine.withMCJIT ctx optimizationLevel codeModel framePointerElimination fastInstructionSelection
 where
  optimizationLevel = Just 2 
  codeModel  = Nothing
  framePointerElimination  = Nothing
  fastInstructionSelection = Nothing

foreign import ccall "dynamic" runFunPtr :: Foreign.Ptr.FunPtr (IO Double) -> IO Double


verifyAndOptimizeModule :: LLVM.Module.Module -> IO ()
verifyAndOptimizeModule modl = do
  LLVM.Analysis.verify modl
  b <- LLVM.PassManager.withPassManager
    ( LLVM.PassManager.PassSetSpec
        { LLVM.PassManager.transforms =
            [ LLVM.Transforms.InstructionCombining,
              LLVM.Transforms.Reassociate,
              LLVM.Transforms.GlobalValueNumbering True,
              LLVM.Transforms.SimplifyControlFlowGraph
            ],
          LLVM.PassManager.dataLayout = Nothing,
          LLVM.PassManager.targetLibraryInfo = Nothing,
          LLVM.PassManager.targetMachine = Nothing
        }
    )
    $ \pm -> LLVM.PassManager.runPassManager pm modl
  putStrLn ("with pass manager returns a bool, whose significance is undocumented: " <> show b)

getFunctionFromMCJIT :: LLVM.AST.Name
                      -> LLVM.Internal.ExecutionEngine.ExecutableModule
                           LLVM.Internal.ExecutionEngine.MCJIT
                      -> IO (Maybe (Foreign.Ptr.FunPtr ()))
getFunctionFromMCJIT name (LLVM.Internal.ExecutionEngine.ExecutableModule (LLVM.Internal.ExecutionEngine.MCJIT r) m) = do
  s <- liftIO $ Data.IORef.readIORef r
  case s of
    LLVM.Internal.ExecutionEngine.Deferred _ -> pure Nothing
    LLVM.Internal.ExecutionEngine.Constructed e -> getFunctionFromExecutableModule name e m

getFunctionFromExecutableModule :: LLVM.AST.Name -> Foreign.Ptr.Ptr  LLVM.Internal.FFI.ExecutionEngine.ExecutionEngine
  -> Foreign.Ptr.Ptr LLVM.Internal.FFI.Module.Module -> IO (Maybe (Foreign.Ptr.FunPtr ()))
getFunctionFromExecutableModule name e = handlePtr <=< LLVM.Internal.FFI.Module.getFirstFunction 
  where 
    nameMatches  a@(LLVM.AST.Name {}) b = a == b
    nameMatches  a b@(LLVM.AST.Name {}) = a == b
    nameMatches  (LLVM.AST.UnName {}) (LLVM.AST.UnName {})  = True -- i carefully make sure there's only one

    handlePtr :: Foreign.Ptr.Ptr LLVM.Internal.FFI.PtrHierarchy.Function -> IO (Maybe (Foreign.Ptr.FunPtr ()))
    handlePtr f = 
      if f == Foreign.Ptr.nullPtr
        then pure Nothing
        else do
          ptrName <- LLVM.Internal.DecodeAST.runDecodeAST $ LLVM.Internal.DecodeAST.getGlobalName f
          Debug.Trace.traceM ("name 1: " <> show ptrName <> ";\nname 2: " <> show name)
          if nameMatches name ptrName then do
            p <- liftIO $ LLVM.Internal.FFI.ExecutionEngine.getPointerToGlobal e (LLVM.Internal.FFI.PtrHierarchy.upCast f)
            if p == Foreign.Ptr.nullPtr then pure Nothing else pure . Just $ Foreign.Ptr.castPtrToFunPtr p
          else loopfrom f
    loopfrom = handlePtr <=< LLVM.Internal.FFI.Module.getNextFunction 