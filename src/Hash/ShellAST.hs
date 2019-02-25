module Hash.ShellAST(ShellAST(..)) where

type Cmd = String
type Args = [String]
type Stdin = Maybe String
type Stdout = Maybe String
type Stderr = Maybe String
data ShellAST = Single Cmd Args Stdin Stdout Stderr | Piped ShellAST ShellAST deriving Show
