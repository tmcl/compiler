module Main where

import Control.Monad.Trans qualified
import Data.Text qualified
import Kaleido.Parser qualified
import System.Console.Haskeline qualified
import Text.Megaparsec qualified


process :: Data.Text.Text -> IO ()
process line = do
  let res = Kaleido.Parser.parseTopLevel line
  case res of
    Left err -> putStrLn $ Text.Megaparsec.errorBundlePretty err
    Right ex -> mapM_ print ex

main :: IO ()
main = System.Console.Haskeline.runInputT System.Console.Haskeline.defaultSettings loop
  where
    loop = do
      minput <- System.Console.Haskeline.getInputLine "ready> "
      case minput of
        Nothing -> System.Console.Haskeline.outputStrLn "Goodbye."
        Just input -> Control.Monad.Trans.liftIO (process $ Data.Text.pack input) >> loop