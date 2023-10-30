{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Main where

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
import LLVM.IRBuilder.Internal.SnocList qualified
import LLVM.IRBuilder.Module qualified
import LLVM.Module qualified
import LLVM.PassManager qualified
import LLVM.Pretty qualified
import LLVM.Transforms qualified
import System.Console.Haskeline qualified
import Text.Megaparsec qualified

newtype BigEvilErrorType
  = ParseError Kaleido.Parser.ParserErrorBundle

process :: Kaleido.CodeGen.State -> Data.Text.Text -> IO Kaleido.CodeGen.State
process initState line =
  handleErrors =<< runExceptT do
    res <-
      Kaleido.Parser.parseTopLevel line
        & first ParseError
        & except
    foldrM @[] @_ @Kaleido.AST.StatementAST @Kaleido.CodeGen.State
      jitKaleidoStatement
      initState
      res
  where
    jitKaleidoStatement ::
      Kaleido.AST.StatementAST ->
      Kaleido.CodeGen.State ->
      ExceptT BigEvilErrorType IO Kaleido.CodeGen.State
    jitKaleidoStatement val startState = liftIO do
      let (endState, (op, defs)) = Kaleido.CodeGen.steppableCodegen startState val
      Data.Text.Lazy.IO.putStrLn $ LLVM.Pretty.ppll op
      forM_ (last (Just <$> defs)) \def -> do
        Data.Text.Lazy.IO.putStrLn . LLVM.Pretty.ppll $ def
        llvmModuleFromDefs defs `printOnlyDefFor ` def
      pure endState

    handleErrors ::
      Either
        BigEvilErrorType
        Kaleido.CodeGen.State ->
      IO Kaleido.CodeGen.State
    handleErrors = \case
      Right newState -> pure newState
      Left (ParseError e) -> initState <$ putStrLn (Text.Megaparsec.errorBundlePretty e)

main :: IO ()
main =
  System.Console.Haskeline.runInputT
    System.Console.Haskeline.defaultSettings
    (loop Kaleido.CodeGen.newEmptyState)
  where
    loop state = do
      minput <- System.Console.Haskeline.getInputLine "ready> "
      case minput of
        Nothing ->
          let definitions = LLVM.IRBuilder.Internal.SnocList.getSnocList . LLVM.IRBuilder.Module.builderDefs $ Kaleido.CodeGen.mbsState state
              modul = llvmModuleFromDefs definitions
           in liftIO $ finalizeModule modul
        Just input -> liftIO (process state $ Data.Text.pack input) >>= loop

llvmModuleFromDefs :: [LLVM.AST.Definition] -> LLVM.AST.Module
llvmModuleFromDefs definitions = 
               LLVM.AST.defaultModule {LLVM.AST.moduleName = "my kaleido jit", LLVM.AST.moduleDefinitions = definitions}

finalizeModule :: LLVM.AST.Module -> IO ()
finalizeModule llvmModule = withSatisfactoryModule llvmModule \modl -> do
  LLVM.Module.moduleLLVMAssembly modl >>= Data.ByteString.putStr

printOnlyDefFor :: LLVM.AST.Module -> LLVM.AST.Definition -> IO ()
printOnlyDefFor inModule def = withSatisfactoryModule inModule \modl -> do
  ast <- LLVM.Module.moduleAST modl
  let fixedDefs = LLVM.AST.moduleDefinitions ast
  mapM_ (Data.Text.Lazy.IO.putStrLn . LLVM.Pretty.ppll) (fixedDefs `findCorrespondingDefinition` def)

findCorrespondingDefinition :: [LLVM.AST.Definition] -> LLVM.AST.Definition -> Maybe LLVM.AST.Definition
findCorrespondingDefinition [] _ = Nothing
findCorrespondingDefinition ((f@(LLVM.AST.GlobalDefinition (LLVM.AST.Function _ _ _ _ _ _ nm1 _ _ _ _ _ _ _ _ _ _)):_)) (LLVM.AST.GlobalDefinition (LLVM.AST.Function _ _ _ _ _ _ nm2 _ _ _ _ _ _ _ _ _ _))
  | (nm1 :: LLVM.AST.Name) == nm2 = Just f
findCorrespondingDefinition (_:rst) def = findCorrespondingDefinition rst def

withSatisfactoryModule :: LLVM.AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withSatisfactoryModule llvmModule ma = LLVM.Context.withContext \ctx -> LLVM.Module.withModuleFromAST ctx llvmModule \modl -> do
  moduleToMySatisfaction modl
  ma modl

moduleToMySatisfaction :: LLVM.Module.Module -> IO ()
moduleToMySatisfaction modl = do
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