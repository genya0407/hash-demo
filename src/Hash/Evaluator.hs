{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import System.Exit (ExitCode(..))
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait, waitPid)

evalAST :: ShellAST -> IO ExitCode
evalAST (Single cmd args) = do
  let searchPath = not ('/' `elem` cmd)
  let env = Nothing
  -- forkしてexec．
  pid <- forkProcess $ do
    executeFile cmd searchPath args env
  -- forkしたプロセスの終了を待つ
  exitcode <- waitPid pid
  return exitcode