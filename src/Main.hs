{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import System.IO
import System.Exit
import Data.Maybe
import Control.Exception
import Hash.Parser (parseLine)
import Hash.Evaluator (evalAST)

main :: IO ()
main = do
  -- プロンプトを表示
  promptString <- fromMaybe "Hash> " <$> lookupEnv "PROMPT"
  putStr promptString
  hFlush stdout

  -- 入力行を受け取る
  line <- catch getLine $ \(_ :: IOException) -> exitWith ExitSuccess

  -- パース & 実行
  case parseLine line of
    Left err -> print err
    Right expr -> do
      evalAST expr
      hFlush stdout

  -- 始めに戻る
  main