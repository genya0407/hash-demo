module Hash.ShellAST(ShellAST(..)) where

type Cmd = String
type Args = [String]
data ShellAST = Single Cmd Args | Piped ShellAST ShellAST deriving Show
