module Hash.Utils(waitPid, forkWait, hDuplicateTo') where

import GHC.IO.Handle (hDuplicateTo)
import System.Exit (ExitCode(..))
import System.Posix.Process (getProcessStatus, forkProcess)
import System.Posix.Types (ProcessID)
import System.Posix.Process (ProcessStatus(Exited))

waitPid :: ProcessID -> IO ExitCode
waitPid pid = do
    status <- getProcessStatus True False pid
    case status of
        Just (Exited exitcode) -> return exitcode
        _ -> undefined -- FIXME

forkWait action = waitPid =<< forkProcess action

hDuplicateTo' h1 h2 = if h1 == h2 then return () else hDuplicateTo h1 h2
    