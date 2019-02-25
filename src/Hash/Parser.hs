module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Hash.ShellAST (ShellAST(..))

parseLine :: String -> Either ParseError ShellAST
parseLine = parse parser ""

parser = parseSingle

parseSingle = do
  spaces
  cmd:args <- many1 (try exprNormalToken >>= (\tk -> spaces >> return tk))
  return $ Single cmd args
  where
    exprNormalToken = many1 $ noneOf " |&;><"
