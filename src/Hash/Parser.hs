module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Hash.ShellAST (ShellAST(..))

parseLine :: String -> Either ParseError ShellAST
parseLine = parse parser ""

parser = parseSingle `chainl1` parsePipe

parsePipe = try $ string "|" >> (lookAhead . try $ noneOf "|") >> return Piped
parseSingle = do
  spaces
  cmd:args <- many1 $ do
    tk <- try $ many1 (noneOf " |&;><")
    spaces
    return tk
  return $ Single cmd args
