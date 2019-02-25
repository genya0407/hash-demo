{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Hash.Parser(parseLine) where

import Text.Parsec
import Text.Parsec.Char
import Hash.ShellAST (ShellAST(..))
import Data.Function ((&))
import Safe (lastMay)

parseLine :: String -> Either ParseError ShellAST
parseLine = parse parser ""

parser = parseSingle `chainl1` parsePipe `chainl1` parseAnd `chainl1` parseOr `chainl1` parseSemi

parsePipe = try $ string "|" >> (lookAhead . try $ noneOf "|") >> return Piped
parseAnd = try $ string "&&" >> return And
parseOr = try $ string "||" >> return Or
parseSemi = try $ string ";" >> return Block

data CmdToken = Stdin String | Stdout String | Stderr String | Other String deriving Show
parseSingle = do
  spaces
  (cmdTokens :: [CmdToken]) <- [parseStdin, parseStdout, parseStderr, parseOther] & map parseFollowedBySpaces & foldr1 (<|>) & many1
  let fnameStdin = lastMay [fname | Stdin fname <- cmdTokens ]
  let fnameStdout = lastMay [fname | Stdout fname <- cmdTokens ]
  let fnameStderr = lastMay [fname | Stderr fname <- cmdTokens ]
  let others = [cmd | Other cmd <- cmdTokens ]
  let cmd = head others
  let args = tail others
  skipMany . try $ string ";" >> spaces >> eof
  return $ Single cmd args fnameStdin fnameStdout fnameStderr
  where
    parseStdin  = parseRedirect "<" Stdin
    parseStdout = parseRedirect ">" Stdout
    parseStderr = parseRedirect "2>" Stderr
    parseRedirect tk constructor = do
      string tk
      spaces
      fname <- many1 (noneOf " |&;><")
      return $ constructor fname
    parseOther = Other <$> parseNormalToken
    parseNormalToken = many1 (noneOf " |&;><")
    parseFollowedBySpaces p = do
      res <- p
      spaces
      return res