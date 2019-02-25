{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import GHC.IO.Handle (Handle, hClose)
import System.IO (stdout, stdin, stderr)
import System.Exit (ExitCode(..))
import System.Process (createPipe)

import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait, hDuplicateTo')

evalAST :: (Handle, Handle) -> ShellAST -> IO ExitCode
evalAST (input, output) (Single cmd args) = do
  -- stdinとstdoutを差し替える
  hDuplicateTo' input stdin
  hDuplicateTo' output stdout

  let searchPath = not ('/' `elem` cmd)
  let env = Nothing
  -- フォークして実行．ExitCodeを返す
  exitcode <- forkWait $ executeFile cmd searchPath args env
  return exitcode

evalAST (input, output) (Piped leftAST rightAST) = do
  (readPipe, writePipe) <- createPipe
  -- フォークして左のASTを実行．
  forkProcess $ do
    hClose readPipe -- こちらのプロセスでは不要なので閉じる
    evalAST (input, writePipe) leftAST
    return ()
  hClose writePipe -- こちらのプロセスでは不要なので閉じる
  evalAST (readPipe, output) rightAST
