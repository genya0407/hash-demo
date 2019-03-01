{-# LANGUAGE ScopedTypeVariables #-}
module Hash.Evaluator(evalAST) where

import GHC.IO.Handle (Handle, hClose, hDuplicate)
import System.IO (stdout, stdin, stderr, hPutStrLn, openFile, IOMode(ReadMode, WriteMode))
import System.Exit (ExitCode(..))
import System.Process (createPipe)
import System.Environment (getEnv)
import System.Directory (setCurrentDirectory)
import System.Posix.Process (forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import Control.Exception (catch, IOException)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Traversable (sequence)
import Safe (headMay)

import Hash.ShellAST (ShellAST(..))
import Hash.Utils (forkWait, hDuplicateTo', waitPid)

--evalAST :: (Handle, Handle) -> ShellAST -> IO ExitCode
--evalAST (input, output) (Single cmd args fStdin fStdout fStderr) = do
evalAST :: ShellAST -> IO ExitCode
evalAST (Single cmd args _ _ _) = do
  let searchPath = not ('/' `elem` cmd)
  let env = Nothing

  pid <- forkProcess $ do
    executeFile cmd searchPath args env
  exitcode <- waitPid pid

  return exitcode

  -- exitcode <- if cmd == "cd"
  --             then do
  --               home <- getEnv "HOME"
  --               let targetDir = fromMaybe home $ headMay args
  --               let cd = setCurrentDirectory targetDir >> return ExitSuccess
  --               let failed = hPutStrLn stderr ("cd: no such file or directory: " ++ targetDir) >> return (ExitFailure 1)
  --               catch cd (\(_ :: IOException) -> restoreFps >> failed)
  --             else
  --               forkWait $ executeFile cmd searchPath args env

-- evalAST (input, output) (Piped leftAST rightAST) = do
--   (readPipe, writePipe) <- createPipe
--   -- フォークして左のASTを実行．
--   forkProcess $ do
--     hClose readPipe -- こちらのプロセスでは不要なので閉じる
--     evalAST (input, writePipe) leftAST
--     hClose writePipe
--     return ()
--   hClose writePipe -- こちらのプロセスでは不要なので閉じる
--   -- 元プロセスで右のASTを実行
--   exitcode <- evalAST (readPipe, output) rightAST
--   hClose readPipe
--   return exitcode

-- evalAST handles (And leftAST rightAST) = do
--   exitcode <- evalAST handles leftAST
--   if exitcode == ExitSuccess
--   then
--     evalAST handles rightAST
--   else
--     return exitcode

-- evalAST handles (Or leftAST rightAST) = do
--   exitcode <- evalAST handles leftAST
--   if exitcode == ExitSuccess
--   then
--     return exitcode
--   else
--     evalAST handles rightAST

-- evalAST handles (Block leftAST rightAST) = do
--   evalAST handles leftAST
--   evalAST handles rightAST
