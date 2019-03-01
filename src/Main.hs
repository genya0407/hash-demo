{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import System.IO
import System.Exit
import Data.Maybe
import Control.Exception
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

import Hash.Parser (parseLine)
import Hash.Evaluator (evalAST)

prompt = fromMaybe "Hash> " <$> lookupEnv "PROMPT"
inputSettings = Settings { complete = completeFilename, historyFile = Nothing, autoAddHistory = True }

main :: IO ()
main = runInputT inputSettings repl
  where
    repl :: InputT IO ()
    repl = do
      -- プロンプトを表示 & 入力行を取得
      minput <- liftIO prompt >>= getInputLine

      -- 行を構文解析 & 評価
      case minput of
        Nothing -> return () -- EOF
        Just "" -> repl
        Just line -> do
          liftIO $ do
            case parseLine line of
              Left err -> print err
              Right ast -> do
                --evalAST (stdin, stdout) ast
                evalAST ast
                hFlush stdout
                return ()
          repl