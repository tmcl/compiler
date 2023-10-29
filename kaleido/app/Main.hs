{-# Language LambdaCase #-}
{-# Language BlockArguments #-}
module Main where

import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Data.Text qualified
import Kaleido.Parser qualified
import System.Console.Haskeline qualified
import Text.Megaparsec qualified
import Data.Function
import Data.Bifunctor 
import LLVM.Pretty qualified
import Kaleido.CodeGen qualified
import Data.Text.Lazy.IO qualified
import Kaleido.AST qualified
import Data.Foldable
import LLVM.AST qualified
import LLVM.IRBuilder.Internal.SnocList qualified
import LLVM.IRBuilder.Module qualified

newtype BigEvilErrorType = 
  ParseError Kaleido.Parser.ParserErrorBundle 

process :: Kaleido.CodeGen.State -> Data.Text.Text -> IO Kaleido.CodeGen.State
process initState line = handleErrors =<< runExceptT do
  res <- Kaleido.Parser.parseTopLevel line
            & first ParseError 
            & except
  foldrM @[] @_ @Kaleido.AST.StatementAST @Kaleido.CodeGen.State 
    action 
    initState
    res
     

  where
    action
        :: Kaleido.AST.StatementAST
           -> Kaleido.CodeGen.State
           -> ExceptT BigEvilErrorType IO Kaleido.CodeGen.State
    action val startState = liftIO do
      print val 
      let (endState, (op, defs)) = Kaleido.CodeGen.steppableCodegen startState val
      Data.Text.Lazy.IO.putStrLn $ LLVM.Pretty.ppll op
      (Data.Text.Lazy.IO.putStrLn . LLVM.Pretty.ppll) `mapM_` last (Just <$> defs)
      pure endState

    handleErrors :: Either
                        BigEvilErrorType
                        Kaleido.CodeGen.State
                      -> IO Kaleido.CodeGen.State
    handleErrors = \case
      Right newState -> pure newState
      Left (ParseError e) -> initState <$ putStrLn (Text.Megaparsec.errorBundlePretty e) 

main :: IO ()
main = System.Console.Haskeline.runInputT System.Console.Haskeline.defaultSettings 
 (loop Kaleido.CodeGen.newEmptyState)
  where
    loop state = do
      minput <- System.Console.Haskeline.getInputLine "ready> "
      case minput of
        Nothing -> 
          let definitions = LLVM.IRBuilder.Internal.SnocList.getSnocList . LLVM.IRBuilder.Module.builderDefs $ Kaleido.CodeGen.mbsState state
              modul = LLVM.AST.defaultModule { LLVM.AST.moduleName = "my kaleido jit", LLVM.AST.moduleDefinitions = definitions }
          in liftIO $ Data.Text.Lazy.IO.putStrLn $ LLVM.Pretty.ppllvm modul
        Just input -> liftIO (process state $ Data.Text.pack input) >>= loop 
