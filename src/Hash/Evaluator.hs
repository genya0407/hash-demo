{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import System.Exit (ExitCode(..))
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait)

evalAST :: ShellAST -> IO ExitCode
evalAST (Single cmd args) = do
  let searchPath = not ('/' `elem` cmd)
  let env = Nothing
  -- フォークして実行．ExitCodeを返す
  exitcode <- forkWait $ executeFile cmd searchPath args env
  return exitcode