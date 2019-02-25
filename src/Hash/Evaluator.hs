{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import System.Exit (ExitCode(..))
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait)

evalAST :: ShellAST -> IO ExitCode
evalAST (Single cmd args) = do
  -- PATHを探索するかどうか．コマンドにスラッシュが含まれていた場合はPATHは見ない．
  let searchPath = not ('/' `elem` cmd)
  -- 環境変数の追加などはサポートしないことにする
  let env = Nothing
  -- フォークして実行．ExitCodeを返す
  exitcode <- forkWait $ executeFile cmd searchPath args env
  return exitcode