{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import GHC.IO.Handle (Handle, hClose)
import System.IO (stdout, stdin, stderr, openFile, IOMode(ReadMode, WriteMode))
import System.Exit (ExitCode(..))
import System.Process (createPipe)
import Data.Maybe (fromMaybe)

import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait, hDuplicateTo')

evalAST :: (Handle, Handle) -> ShellAST -> IO ExitCode
evalAST (input, output) (Single cmd args fStdin fStdout fStderr) = do
  -- stdinとstdoutとstderrを差し替える
  input' <- fromMaybe (return input) (flip openFile ReadMode <$> fStdin)
  hDuplicateTo' input' stdin

  output' <- fromMaybe (return output) (flip openFile WriteMode <$> fStdout)
  hDuplicateTo' output' stdout

  err' <- fromMaybe (return stderr) (flip openFile WriteMode <$> fStderr)
  hDuplicateTo' err' stderr

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
